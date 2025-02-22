/* Subroutines used for expanding LoongArch builtins.
   Copyright (C) 2021-2025 Free Software Foundation, Inc.
   Contributed by Loongson Ltd.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#define IN_TARGET_CODE 1

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "target.h"
#include "rtl.h"
#include "tree.h"
#include "memmodel.h"
#include "gimple.h"
#include "tm_p.h"
#include "optabs.h"
#include "recog.h"
#include "diagnostic.h"
#include "fold-const.h"
#include "explow.h"
#include "expr.h"
#include "langhooks.h"
#include "emit-rtl.h"
#include "case-cfn-macros.h"

/* Macros to create an enumeration identifier for a function prototype.  */
#define LARCH_FTYPE_NAME1(A, B) LARCH_##A##_FTYPE_##B
#define LARCH_FTYPE_NAME2(A, B, C) LARCH_##A##_FTYPE_##B##_##C
#define LARCH_FTYPE_NAME3(A, B, C, D) LARCH_##A##_FTYPE_##B##_##C##_##D
#define LARCH_FTYPE_NAME4(A, B, C, D, E) \
  LARCH_##A##_FTYPE_##B##_##C##_##D##_##E

/* Classifies the prototype of a built-in function.  */
enum loongarch_function_type
{
#define DEF_LARCH_FTYPE(NARGS, LIST) LARCH_FTYPE_NAME##NARGS LIST,
#include "config/loongarch/loongarch-ftypes.def"
#undef DEF_LARCH_FTYPE
  LARCH_MAX_FTYPE_MAX
};

/* Specifies how a built-in function should be converted into rtl.  */
enum loongarch_builtin_type
{
  /* The function corresponds directly to an .md pattern.  The return
     value is mapped to operand 0 and the arguments are mapped to
     operands 1 and above.  */
  LARCH_BUILTIN_DIRECT,

  /* The function corresponds directly to an .md pattern.  There is no return
     value and the arguments are mapped to operands 0 and above.  */
  LARCH_BUILTIN_DIRECT_NO_TARGET,

  /* For generating LoongArch LSX.  */
  LARCH_BUILTIN_LSX,

  /* The function corresponds to an LSX conditional branch instruction
     combined with a compare instruction.  */
  LARCH_BUILTIN_LSX_TEST_BRANCH,

  /* For generating LoongArch LASX.  */
  LARCH_BUILTIN_LASX,

  /* The function corresponds to an LASX conditional branch instruction
     combined with a compare instruction.  */
  LARCH_BUILTIN_LASX_TEST_BRANCH,
};

/* Declare an availability predicate for built-in functions that require
 * COND to be true.  NAME is the main part of the predicate's name.  */
#define AVAIL_ALL(NAME, COND) \
  static unsigned int \
  loongarch_builtin_avail_##NAME (void) \
  { \
    return (COND) ? 1 : 0; \
  }

static unsigned int
loongarch_builtin_avail_default (void)
{
  return 1;
}
/* This structure describes a single built-in function.  */
struct loongarch_builtin_description
{
  /* The code of the main .md file instruction.  See loongarch_builtin_type
     for more information.  */
  enum insn_code icode;

  /* The name of the built-in function.  */
  const char *name;

  /* Specifies how the function should be expanded.  */
  enum loongarch_builtin_type builtin_type;

  /* The function's prototype.  */
  enum loongarch_function_type function_type;

  /* Whether the function is available.  */
  unsigned int (*avail) (void);
};

AVAIL_ALL (hard_float, TARGET_HARD_FLOAT_ABI)
AVAIL_ALL (lsx, ISA_HAS_LSX)
AVAIL_ALL (lasx, ISA_HAS_LASX)
AVAIL_ALL (frecipe, ISA_HAS_FRECIPE && TARGET_HARD_FLOAT_ABI)
AVAIL_ALL (lsx_frecipe, ISA_HAS_LSX && ISA_HAS_FRECIPE)
AVAIL_ALL (lasx_frecipe, ISA_HAS_LASX && ISA_HAS_FRECIPE)

/* Construct a loongarch_builtin_description from the given arguments.

   INSN is the name of the associated instruction pattern, without the
   leading CODE_FOR_loongarch_.

   CODE is the floating-point condition code associated with the
   function.  It can be 'f' if the field is not applicable.

   NAME is the name of the function itself, without the leading
   "__builtin_loongarch_".

   BUILTIN_TYPE and FUNCTION_TYPE are loongarch_builtin_description fields.

   AVAIL is the name of the availability predicate, without the leading
   loongarch_builtin_avail_.  */
#define LARCH_BUILTIN(INSN, NAME, BUILTIN_TYPE, FUNCTION_TYPE, AVAIL) \
  { \
    CODE_FOR_loongarch_##INSN, "__builtin_loongarch_" NAME, \
    BUILTIN_TYPE, FUNCTION_TYPE, \
    loongarch_builtin_avail_##AVAIL \
  }

/* Define __builtin_loongarch_<INSN>, which is a LARCH_BUILTIN_DIRECT function
   mapped to instruction CODE_FOR_loongarch_<INSN>,  FUNCTION_TYPE and AVAIL
   are as for LARCH_BUILTIN.  */
#define DIRECT_BUILTIN(INSN, FUNCTION_TYPE, AVAIL) \
  LARCH_BUILTIN (INSN, #INSN, LARCH_BUILTIN_DIRECT, FUNCTION_TYPE, AVAIL)

/* Define __builtin_loongarch_<INSN>, which is a LARCH_BUILTIN_DIRECT_NO_TARGET
   function mapped to instruction CODE_FOR_loongarch_<INSN>,  FUNCTION_TYPE
   and AVAIL are as for LARCH_BUILTIN.  */
#define DIRECT_NO_TARGET_BUILTIN(INSN, FUNCTION_TYPE, AVAIL) \
  LARCH_BUILTIN (INSN, #INSN, LARCH_BUILTIN_DIRECT_NO_TARGET, \
		 FUNCTION_TYPE, AVAIL)

/* Define an LSX LARCH_BUILTIN_DIRECT function __builtin_lsx_<INSN>
   for instruction CODE_FOR_lsx_<INSN>.  FUNCTION_TYPE is a builtin_description
   field.  */
#define LSX_BUILTIN(INSN, FUNCTION_TYPE)				\
  { CODE_FOR_lsx_ ## INSN,						\
    "__builtin_lsx_" #INSN,  LARCH_BUILTIN_DIRECT,			\
    FUNCTION_TYPE, loongarch_builtin_avail_lsx }

 /* Define an LSX LARCH_BUILTIN_DIRECT function __builtin_lsx_<INSN>
    for instruction CODE_FOR_lsx_<INSN>.  FUNCTION_TYPE is a builtin_description
    field. AVAIL is the name of the availability predicate, without the leading
    loongarch_builtin_avail_.  */
#define LSX_EXT_BUILTIN(INSN, FUNCTION_TYPE, AVAIL)                     \
  { CODE_FOR_lsx_ ## INSN,                                              \
    "__builtin_lsx_" #INSN,  LARCH_BUILTIN_DIRECT,                      \
    FUNCTION_TYPE, loongarch_builtin_avail_##AVAIL }


/* Define an LSX LARCH_BUILTIN_LSX_TEST_BRANCH function __builtin_lsx_<INSN>
   for instruction CODE_FOR_lsx_<INSN>.  FUNCTION_TYPE is a builtin_description
   field.  */
#define LSX_BUILTIN_TEST_BRANCH(INSN, FUNCTION_TYPE)			\
  { CODE_FOR_lsx_ ## INSN,						\
    "__builtin_lsx_" #INSN, LARCH_BUILTIN_LSX_TEST_BRANCH,		\
    FUNCTION_TYPE, loongarch_builtin_avail_lsx }

/* Define an LSX LARCH_BUILTIN_DIRECT_NO_TARGET function __builtin_lsx_<INSN>
   for instruction CODE_FOR_lsx_<INSN>.  FUNCTION_TYPE is a builtin_description
   field.  */
#define LSX_NO_TARGET_BUILTIN(INSN, FUNCTION_TYPE)			\
  { CODE_FOR_lsx_ ## INSN,						\
    "__builtin_lsx_" #INSN,  LARCH_BUILTIN_DIRECT_NO_TARGET,		\
    FUNCTION_TYPE, loongarch_builtin_avail_lsx }

/* Define an LASX LARCH_BUILTIN_DIRECT function __builtin_lasx_<INSN>
   for instruction CODE_FOR_lasx_<INSN>.  FUNCTION_TYPE is a builtin_description
   field.  */
#define LASX_BUILTIN(INSN, FUNCTION_TYPE)				\
  { CODE_FOR_lasx_ ## INSN,						\
    "__builtin_lasx_" #INSN,  LARCH_BUILTIN_LASX,			\
    FUNCTION_TYPE, loongarch_builtin_avail_lasx }

/* Define an LASX LARCH_BUILTIN_DIRECT function __builtin_lasx_<INSN>
   for instruction CODE_FOR_lasx_<INSN>.  FUNCTION_TYPE is a builtin_description
   field. AVAIL is the name of the availability predicate, without the leading
   loongarch_builtin_avail_.  */
#define LASX_EXT_BUILTIN(INSN, FUNCTION_TYPE, AVAIL)                    \
  { CODE_FOR_lasx_ ## INSN,                                             \
    "__builtin_lasx_" #INSN,  LARCH_BUILTIN_LASX,                       \
    FUNCTION_TYPE, loongarch_builtin_avail_##AVAIL }

/* Define an LASX LARCH_BUILTIN_DIRECT_NO_TARGET function __builtin_lasx_<INSN>
   for instruction CODE_FOR_lasx_<INSN>.  FUNCTION_TYPE is a builtin_description
   field.  */
#define LASX_NO_TARGET_BUILTIN(INSN, FUNCTION_TYPE)			\
  { CODE_FOR_lasx_ ## INSN,						\
    "__builtin_lasx_" #INSN,  LARCH_BUILTIN_DIRECT_NO_TARGET,		\
    FUNCTION_TYPE, loongarch_builtin_avail_lasx }

/* Define an LASX LARCH_BUILTIN_LASX_TEST_BRANCH function __builtin_lasx_<INSN>
   for instruction CODE_FOR_lasx_<INSN>.  FUNCTION_TYPE is a builtin_description
   field.  */
#define LASX_BUILTIN_TEST_BRANCH(INSN, FUNCTION_TYPE)			\
  { CODE_FOR_lasx_ ## INSN,						\
    "__builtin_lasx_" #INSN, LARCH_BUILTIN_LASX_TEST_BRANCH,		\
    FUNCTION_TYPE, loongarch_builtin_avail_lasx }

/* LoongArch SX define CODE_FOR_lsx_xxx */
#define CODE_FOR_lsx_vsadd_b CODE_FOR_ssaddv16qi3
#define CODE_FOR_lsx_vsadd_h CODE_FOR_ssaddv8hi3
#define CODE_FOR_lsx_vsadd_w CODE_FOR_ssaddv4si3
#define CODE_FOR_lsx_vsadd_d CODE_FOR_ssaddv2di3
#define CODE_FOR_lsx_vsadd_bu CODE_FOR_usaddv16qi3
#define CODE_FOR_lsx_vsadd_hu CODE_FOR_usaddv8hi3
#define CODE_FOR_lsx_vsadd_wu CODE_FOR_usaddv4si3
#define CODE_FOR_lsx_vsadd_du CODE_FOR_usaddv2di3
#define CODE_FOR_lsx_vadd_b CODE_FOR_addv16qi3
#define CODE_FOR_lsx_vadd_h CODE_FOR_addv8hi3
#define CODE_FOR_lsx_vadd_w CODE_FOR_addv4si3
#define CODE_FOR_lsx_vadd_d CODE_FOR_addv2di3
#define CODE_FOR_lsx_vaddi_bu CODE_FOR_addv16qi3
#define CODE_FOR_lsx_vaddi_hu CODE_FOR_addv8hi3
#define CODE_FOR_lsx_vaddi_wu CODE_FOR_addv4si3
#define CODE_FOR_lsx_vaddi_du CODE_FOR_addv2di3
#define CODE_FOR_lsx_vand_v CODE_FOR_andv16qi3
#define CODE_FOR_lsx_vandi_b CODE_FOR_andv16qi3
#define CODE_FOR_lsx_bnz_v CODE_FOR_lsx_bnz_v_b
#define CODE_FOR_lsx_bz_v CODE_FOR_lsx_bz_v_b
#define CODE_FOR_lsx_vbitsel_v CODE_FOR_lsx_vbitsel_b
#define CODE_FOR_lsx_vseqi_b CODE_FOR_lsx_vseq_b
#define CODE_FOR_lsx_vseqi_h CODE_FOR_lsx_vseq_h
#define CODE_FOR_lsx_vseqi_w CODE_FOR_lsx_vseq_w
#define CODE_FOR_lsx_vseqi_d CODE_FOR_lsx_vseq_d
#define CODE_FOR_lsx_vslti_b CODE_FOR_lsx_vslt_b
#define CODE_FOR_lsx_vslti_h CODE_FOR_lsx_vslt_h
#define CODE_FOR_lsx_vslti_w CODE_FOR_lsx_vslt_w
#define CODE_FOR_lsx_vslti_d CODE_FOR_lsx_vslt_d
#define CODE_FOR_lsx_vslti_bu CODE_FOR_lsx_vslt_bu
#define CODE_FOR_lsx_vslti_hu CODE_FOR_lsx_vslt_hu
#define CODE_FOR_lsx_vslti_wu CODE_FOR_lsx_vslt_wu
#define CODE_FOR_lsx_vslti_du CODE_FOR_lsx_vslt_du
#define CODE_FOR_lsx_vslei_b CODE_FOR_lsx_vsle_b
#define CODE_FOR_lsx_vslei_h CODE_FOR_lsx_vsle_h
#define CODE_FOR_lsx_vslei_w CODE_FOR_lsx_vsle_w
#define CODE_FOR_lsx_vslei_d CODE_FOR_lsx_vsle_d
#define CODE_FOR_lsx_vslei_bu CODE_FOR_lsx_vsle_bu
#define CODE_FOR_lsx_vslei_hu CODE_FOR_lsx_vsle_hu
#define CODE_FOR_lsx_vslei_wu CODE_FOR_lsx_vsle_wu
#define CODE_FOR_lsx_vslei_du CODE_FOR_lsx_vsle_du
#define CODE_FOR_lsx_vdiv_b CODE_FOR_divv16qi3
#define CODE_FOR_lsx_vdiv_h CODE_FOR_divv8hi3
#define CODE_FOR_lsx_vdiv_w CODE_FOR_divv4si3
#define CODE_FOR_lsx_vdiv_d CODE_FOR_divv2di3
#define CODE_FOR_lsx_vdiv_bu CODE_FOR_udivv16qi3
#define CODE_FOR_lsx_vdiv_hu CODE_FOR_udivv8hi3
#define CODE_FOR_lsx_vdiv_wu CODE_FOR_udivv4si3
#define CODE_FOR_lsx_vdiv_du CODE_FOR_udivv2di3
#define CODE_FOR_lsx_vfadd_s CODE_FOR_addv4sf3
#define CODE_FOR_lsx_vfadd_d CODE_FOR_addv2df3
#define CODE_FOR_lsx_vftintrz_w_s CODE_FOR_fix_truncv4sfv4si2
#define CODE_FOR_lsx_vftintrz_l_d CODE_FOR_fix_truncv2dfv2di2
#define CODE_FOR_lsx_vftintrz_wu_s CODE_FOR_fixuns_truncv4sfv4si2
#define CODE_FOR_lsx_vftintrz_lu_d CODE_FOR_fixuns_truncv2dfv2di2
#define CODE_FOR_lsx_vftintrz_w_d CODE_FOR_vec_pack_sfix_trunc_v2df
#define CODE_FOR_lsx_vftintrzh_l_s CODE_FOR_vec_unpack_sfix_trunc_hi_v4sf
#define CODE_FOR_lsx_vftintrzl_l_s CODE_FOR_vec_unpack_sfix_trunc_lo_v4sf
#define CODE_FOR_lsx_vffint_s_w CODE_FOR_floatv4siv4sf2
#define CODE_FOR_lsx_vffint_d_l CODE_FOR_floatv2div2df2
#define CODE_FOR_lsx_vffint_s_wu CODE_FOR_floatunsv4siv4sf2
#define CODE_FOR_lsx_vffint_d_lu CODE_FOR_floatunsv2div2df2
#define CODE_FOR_lsx_vffint_s_l CODE_FOR_vec_packs_float_v2di
#define CODE_FOR_lsx_vffinth_d_w CODE_FOR_vec_unpacks_float_hi_v4si
#define CODE_FOR_lsx_vffintl_d_w CODE_FOR_vec_unpacks_float_lo_v4si
#define CODE_FOR_lsx_vexth_h_b CODE_FOR_vec_unpacks_hi_v16qi
#define CODE_FOR_lsx_vexth_w_h CODE_FOR_vec_unpacks_hi_v8hi
#define CODE_FOR_lsx_vexth_d_w CODE_FOR_vec_unpacks_hi_v4si
#define CODE_FOR_lsx_vexth_hu_bu CODE_FOR_vec_unpacku_hi_v16qi
#define CODE_FOR_lsx_vexth_wu_hu CODE_FOR_vec_unpacku_hi_v8hi
#define CODE_FOR_lsx_vexth_du_wu CODE_FOR_vec_unpacku_hi_v4si
#define CODE_FOR_lsx_vfcvth_d_s CODE_FOR_vec_unpacks_hi_v4sf
#define CODE_FOR_lsx_vfcvtl_d_s CODE_FOR_vec_unpacks_lo_v4sf
#define CODE_FOR_lsx_vfsub_s CODE_FOR_subv4sf3
#define CODE_FOR_lsx_vfsub_d CODE_FOR_subv2df3
#define CODE_FOR_lsx_vfmul_s CODE_FOR_mulv4sf3
#define CODE_FOR_lsx_vfmul_d CODE_FOR_mulv2df3
#define CODE_FOR_lsx_vfdiv_s CODE_FOR_divv4sf3
#define CODE_FOR_lsx_vfdiv_d CODE_FOR_divv2df3
#define CODE_FOR_lsx_vfmax_s CODE_FOR_smaxv4sf3
#define CODE_FOR_lsx_vfmax_d CODE_FOR_smaxv2df3
#define CODE_FOR_lsx_vfmin_s CODE_FOR_sminv4sf3
#define CODE_FOR_lsx_vfmin_d CODE_FOR_sminv2df3
#define CODE_FOR_lsx_vfsqrt_s CODE_FOR_sqrtv4sf2
#define CODE_FOR_lsx_vfsqrt_d CODE_FOR_sqrtv2df2
#define CODE_FOR_lsx_vflogb_s CODE_FOR_logbv4sf2
#define CODE_FOR_lsx_vflogb_d CODE_FOR_logbv2df2
#define CODE_FOR_lsx_vmax_b CODE_FOR_smaxv16qi3
#define CODE_FOR_lsx_vmax_h CODE_FOR_smaxv8hi3
#define CODE_FOR_lsx_vmax_w CODE_FOR_smaxv4si3
#define CODE_FOR_lsx_vmax_d CODE_FOR_smaxv2di3
#define CODE_FOR_lsx_vmaxi_b CODE_FOR_smaxv16qi3
#define CODE_FOR_lsx_vmaxi_h CODE_FOR_smaxv8hi3
#define CODE_FOR_lsx_vmaxi_w CODE_FOR_smaxv4si3
#define CODE_FOR_lsx_vmaxi_d CODE_FOR_smaxv2di3
#define CODE_FOR_lsx_vmax_bu CODE_FOR_umaxv16qi3
#define CODE_FOR_lsx_vmax_hu CODE_FOR_umaxv8hi3
#define CODE_FOR_lsx_vmax_wu CODE_FOR_umaxv4si3
#define CODE_FOR_lsx_vmax_du CODE_FOR_umaxv2di3
#define CODE_FOR_lsx_vmaxi_bu CODE_FOR_umaxv16qi3
#define CODE_FOR_lsx_vmaxi_hu CODE_FOR_umaxv8hi3
#define CODE_FOR_lsx_vmaxi_wu CODE_FOR_umaxv4si3
#define CODE_FOR_lsx_vmaxi_du CODE_FOR_umaxv2di3
#define CODE_FOR_lsx_vmin_b CODE_FOR_sminv16qi3
#define CODE_FOR_lsx_vmin_h CODE_FOR_sminv8hi3
#define CODE_FOR_lsx_vmin_w CODE_FOR_sminv4si3
#define CODE_FOR_lsx_vmin_d CODE_FOR_sminv2di3
#define CODE_FOR_lsx_vmini_b CODE_FOR_sminv16qi3
#define CODE_FOR_lsx_vmini_h CODE_FOR_sminv8hi3
#define CODE_FOR_lsx_vmini_w CODE_FOR_sminv4si3
#define CODE_FOR_lsx_vmini_d CODE_FOR_sminv2di3
#define CODE_FOR_lsx_vmin_bu CODE_FOR_uminv16qi3
#define CODE_FOR_lsx_vmin_hu CODE_FOR_uminv8hi3
#define CODE_FOR_lsx_vmin_wu CODE_FOR_uminv4si3
#define CODE_FOR_lsx_vmin_du CODE_FOR_uminv2di3
#define CODE_FOR_lsx_vmini_bu CODE_FOR_uminv16qi3
#define CODE_FOR_lsx_vmini_hu CODE_FOR_uminv8hi3
#define CODE_FOR_lsx_vmini_wu CODE_FOR_uminv4si3
#define CODE_FOR_lsx_vmini_du CODE_FOR_uminv2di3
#define CODE_FOR_lsx_vmod_b CODE_FOR_modv16qi3
#define CODE_FOR_lsx_vmod_h CODE_FOR_modv8hi3
#define CODE_FOR_lsx_vmod_w CODE_FOR_modv4si3
#define CODE_FOR_lsx_vmod_d CODE_FOR_modv2di3
#define CODE_FOR_lsx_vmod_bu CODE_FOR_umodv16qi3
#define CODE_FOR_lsx_vmod_hu CODE_FOR_umodv8hi3
#define CODE_FOR_lsx_vmod_wu CODE_FOR_umodv4si3
#define CODE_FOR_lsx_vmod_du CODE_FOR_umodv2di3
#define CODE_FOR_lsx_vmuh_b CODE_FOR_smulv16qi3_highpart
#define CODE_FOR_lsx_vmuh_h CODE_FOR_smulv8hi3_highpart
#define CODE_FOR_lsx_vmuh_w CODE_FOR_smulv4si3_highpart
#define CODE_FOR_lsx_vmuh_d CODE_FOR_smulv2di3_highpart
#define CODE_FOR_lsx_vmuh_bu CODE_FOR_umulv16qi3_highpart
#define CODE_FOR_lsx_vmuh_hu CODE_FOR_umulv8hi3_highpart
#define CODE_FOR_lsx_vmuh_wu CODE_FOR_umulv4si3_highpart
#define CODE_FOR_lsx_vmuh_du CODE_FOR_umulv2di3_highpart
#define CODE_FOR_lsx_vmul_b CODE_FOR_mulv16qi3
#define CODE_FOR_lsx_vmul_h CODE_FOR_mulv8hi3
#define CODE_FOR_lsx_vmul_w CODE_FOR_mulv4si3
#define CODE_FOR_lsx_vmul_d CODE_FOR_mulv2di3
#define CODE_FOR_lsx_vclz_b CODE_FOR_clzv16qi2
#define CODE_FOR_lsx_vclz_h CODE_FOR_clzv8hi2
#define CODE_FOR_lsx_vclz_w CODE_FOR_clzv4si2
#define CODE_FOR_lsx_vclz_d CODE_FOR_clzv2di2
#define CODE_FOR_lsx_vnor_v CODE_FOR_lsx_nor_b
#define CODE_FOR_lsx_vor_v CODE_FOR_iorv16qi3
#define CODE_FOR_lsx_vori_b CODE_FOR_iorv16qi3
#define CODE_FOR_lsx_vnori_b CODE_FOR_lsx_nor_b
#define CODE_FOR_lsx_vpcnt_b CODE_FOR_popcountv16qi2
#define CODE_FOR_lsx_vpcnt_h CODE_FOR_popcountv8hi2
#define CODE_FOR_lsx_vpcnt_w CODE_FOR_popcountv4si2
#define CODE_FOR_lsx_vpcnt_d CODE_FOR_popcountv2di2
#define CODE_FOR_lsx_vxor_v CODE_FOR_xorv16qi3
#define CODE_FOR_lsx_vxori_b CODE_FOR_xorv16qi3
#define CODE_FOR_lsx_vsll_b CODE_FOR_vashlv16qi3
#define CODE_FOR_lsx_vsll_h CODE_FOR_vashlv8hi3
#define CODE_FOR_lsx_vsll_w CODE_FOR_vashlv4si3
#define CODE_FOR_lsx_vsll_d CODE_FOR_vashlv2di3
#define CODE_FOR_lsx_vslli_b CODE_FOR_vashlv16qi3
#define CODE_FOR_lsx_vslli_h CODE_FOR_vashlv8hi3
#define CODE_FOR_lsx_vslli_w CODE_FOR_vashlv4si3
#define CODE_FOR_lsx_vslli_d CODE_FOR_vashlv2di3
#define CODE_FOR_lsx_vsra_b CODE_FOR_vashrv16qi3
#define CODE_FOR_lsx_vsra_h CODE_FOR_vashrv8hi3
#define CODE_FOR_lsx_vsra_w CODE_FOR_vashrv4si3
#define CODE_FOR_lsx_vsra_d CODE_FOR_vashrv2di3
#define CODE_FOR_lsx_vsrai_b CODE_FOR_vashrv16qi3
#define CODE_FOR_lsx_vsrai_h CODE_FOR_vashrv8hi3
#define CODE_FOR_lsx_vsrai_w CODE_FOR_vashrv4si3
#define CODE_FOR_lsx_vsrai_d CODE_FOR_vashrv2di3
#define CODE_FOR_lsx_vsrl_b CODE_FOR_vlshrv16qi3
#define CODE_FOR_lsx_vsrl_h CODE_FOR_vlshrv8hi3
#define CODE_FOR_lsx_vsrl_w CODE_FOR_vlshrv4si3
#define CODE_FOR_lsx_vsrl_d CODE_FOR_vlshrv2di3
#define CODE_FOR_lsx_vsrli_b CODE_FOR_vlshrv16qi3
#define CODE_FOR_lsx_vsrli_h CODE_FOR_vlshrv8hi3
#define CODE_FOR_lsx_vsrli_w CODE_FOR_vlshrv4si3
#define CODE_FOR_lsx_vsrli_d CODE_FOR_vlshrv2di3
#define CODE_FOR_lsx_vrotr_b CODE_FOR_vrotrv16qi3
#define CODE_FOR_lsx_vrotr_h CODE_FOR_vrotrv8hi3
#define CODE_FOR_lsx_vrotr_w CODE_FOR_vrotrv4si3
#define CODE_FOR_lsx_vrotr_d CODE_FOR_vrotrv2di3
#define CODE_FOR_lsx_vrotri_b CODE_FOR_rotrv16qi3
#define CODE_FOR_lsx_vrotri_h CODE_FOR_rotrv8hi3
#define CODE_FOR_lsx_vrotri_w CODE_FOR_rotrv4si3
#define CODE_FOR_lsx_vrotri_d CODE_FOR_rotrv2di3
#define CODE_FOR_lsx_vsub_b CODE_FOR_subv16qi3
#define CODE_FOR_lsx_vsub_h CODE_FOR_subv8hi3
#define CODE_FOR_lsx_vsub_w CODE_FOR_subv4si3
#define CODE_FOR_lsx_vsub_d CODE_FOR_subv2di3
#define CODE_FOR_lsx_vsubi_bu CODE_FOR_subv16qi3
#define CODE_FOR_lsx_vsubi_hu CODE_FOR_subv8hi3
#define CODE_FOR_lsx_vsubi_wu CODE_FOR_subv4si3
#define CODE_FOR_lsx_vsubi_du CODE_FOR_subv2di3

#define CODE_FOR_lsx_vpackod_d CODE_FOR_lsx_vilvh_d
#define CODE_FOR_lsx_vpackev_d CODE_FOR_lsx_vilvl_d
#define CODE_FOR_lsx_vpickod_d CODE_FOR_lsx_vilvh_d
#define CODE_FOR_lsx_vpickev_d CODE_FOR_lsx_vilvl_d

#define CODE_FOR_lsx_vrepli_b CODE_FOR_lsx_vrepliv16qi
#define CODE_FOR_lsx_vrepli_h CODE_FOR_lsx_vrepliv8hi
#define CODE_FOR_lsx_vrepli_w CODE_FOR_lsx_vrepliv4si
#define CODE_FOR_lsx_vrepli_d CODE_FOR_lsx_vrepliv2di
#define CODE_FOR_lsx_vsat_b CODE_FOR_lsx_vsat_s_b
#define CODE_FOR_lsx_vsat_h CODE_FOR_lsx_vsat_s_h
#define CODE_FOR_lsx_vsat_w CODE_FOR_lsx_vsat_s_w
#define CODE_FOR_lsx_vsat_d CODE_FOR_lsx_vsat_s_d
#define CODE_FOR_lsx_vsat_bu CODE_FOR_lsx_vsat_u_bu
#define CODE_FOR_lsx_vsat_hu CODE_FOR_lsx_vsat_u_hu
#define CODE_FOR_lsx_vsat_wu CODE_FOR_lsx_vsat_u_wu
#define CODE_FOR_lsx_vsat_du CODE_FOR_lsx_vsat_u_du
#define CODE_FOR_lsx_vavg_b CODE_FOR_lsx_vavg_s_b
#define CODE_FOR_lsx_vavg_h CODE_FOR_lsx_vavg_s_h
#define CODE_FOR_lsx_vavg_w CODE_FOR_lsx_vavg_s_w
#define CODE_FOR_lsx_vavg_d CODE_FOR_lsx_vavg_s_d
#define CODE_FOR_lsx_vavg_bu CODE_FOR_lsx_vavg_u_bu
#define CODE_FOR_lsx_vavg_hu CODE_FOR_lsx_vavg_u_hu
#define CODE_FOR_lsx_vavg_wu CODE_FOR_lsx_vavg_u_wu
#define CODE_FOR_lsx_vavg_du CODE_FOR_lsx_vavg_u_du
#define CODE_FOR_lsx_vavgr_b CODE_FOR_lsx_vavgr_s_b
#define CODE_FOR_lsx_vavgr_h CODE_FOR_lsx_vavgr_s_h
#define CODE_FOR_lsx_vavgr_w CODE_FOR_lsx_vavgr_s_w
#define CODE_FOR_lsx_vavgr_d CODE_FOR_lsx_vavgr_s_d
#define CODE_FOR_lsx_vavgr_bu CODE_FOR_lsx_vavgr_u_bu
#define CODE_FOR_lsx_vavgr_hu CODE_FOR_lsx_vavgr_u_hu
#define CODE_FOR_lsx_vavgr_wu CODE_FOR_lsx_vavgr_u_wu
#define CODE_FOR_lsx_vavgr_du CODE_FOR_lsx_vavgr_u_du
#define CODE_FOR_lsx_vssub_b CODE_FOR_lsx_vssub_s_b
#define CODE_FOR_lsx_vssub_h CODE_FOR_lsx_vssub_s_h
#define CODE_FOR_lsx_vssub_w CODE_FOR_lsx_vssub_s_w
#define CODE_FOR_lsx_vssub_d CODE_FOR_lsx_vssub_s_d
#define CODE_FOR_lsx_vssub_bu CODE_FOR_lsx_vssub_u_bu
#define CODE_FOR_lsx_vssub_hu CODE_FOR_lsx_vssub_u_hu
#define CODE_FOR_lsx_vssub_wu CODE_FOR_lsx_vssub_u_wu
#define CODE_FOR_lsx_vssub_du CODE_FOR_lsx_vssub_u_du
#define CODE_FOR_lsx_vabsd_b CODE_FOR_sabdv16qi3
#define CODE_FOR_lsx_vabsd_h CODE_FOR_sabdv8hi3
#define CODE_FOR_lsx_vabsd_w CODE_FOR_sabdv4si3
#define CODE_FOR_lsx_vabsd_d CODE_FOR_sabdv2di3
#define CODE_FOR_lsx_vabsd_bu CODE_FOR_uabdv16qi3
#define CODE_FOR_lsx_vabsd_hu CODE_FOR_uabdv8hi3
#define CODE_FOR_lsx_vabsd_wu CODE_FOR_uabdv4si3
#define CODE_FOR_lsx_vabsd_du CODE_FOR_uabdv2di3
#define CODE_FOR_lsx_vftint_wu_s CODE_FOR_lsx_vftint_u_wu_s
#define CODE_FOR_lsx_vftint_lu_d CODE_FOR_lsx_vftint_u_lu_d
#define CODE_FOR_lsx_vandn_v CODE_FOR_andnv16qi3
#define CODE_FOR_lsx_vorn_v CODE_FOR_iornv16qi3
#define CODE_FOR_lsx_vneg_b CODE_FOR_vnegv16qi2
#define CODE_FOR_lsx_vneg_h CODE_FOR_vnegv8hi2
#define CODE_FOR_lsx_vneg_w CODE_FOR_vnegv4si2
#define CODE_FOR_lsx_vneg_d CODE_FOR_vnegv2di2
#define CODE_FOR_lsx_vshuf4i_d CODE_FOR_lsx_vshuf4i_d
#define CODE_FOR_lsx_vbsrl_v CODE_FOR_lsx_vbsrl_b
#define CODE_FOR_lsx_vbsll_v CODE_FOR_lsx_vbsll_b
#define CODE_FOR_lsx_vfmadd_s CODE_FOR_fmav4sf4
#define CODE_FOR_lsx_vfmadd_d CODE_FOR_fmav2df4
#define CODE_FOR_lsx_vfmsub_s CODE_FOR_fmsv4sf4
#define CODE_FOR_lsx_vfmsub_d CODE_FOR_fmsv2df4
#define CODE_FOR_lsx_vfnmadd_s CODE_FOR_vfnmaddv4sf4_nmadd4
#define CODE_FOR_lsx_vfnmadd_d CODE_FOR_vfnmaddv2df4_nmadd4
#define CODE_FOR_lsx_vfnmsub_s CODE_FOR_vfnmsubv4sf4_nmsub4
#define CODE_FOR_lsx_vfnmsub_d CODE_FOR_vfnmsubv2df4_nmsub4

#define CODE_FOR_lsx_vsllwil_h_b CODE_FOR_lsx_vsllwil_s_h_b
#define CODE_FOR_lsx_vsllwil_w_h CODE_FOR_lsx_vsllwil_s_w_h
#define CODE_FOR_lsx_vsllwil_d_w CODE_FOR_lsx_vsllwil_s_d_w
#define CODE_FOR_lsx_vsllwil_hu_bu CODE_FOR_lsx_vsllwil_u_hu_bu
#define CODE_FOR_lsx_vsllwil_wu_hu CODE_FOR_lsx_vsllwil_u_wu_hu
#define CODE_FOR_lsx_vsllwil_du_wu CODE_FOR_lsx_vsllwil_u_du_wu
#define CODE_FOR_lsx_vssran_b_h CODE_FOR_lsx_vssran_s_b_h
#define CODE_FOR_lsx_vssran_h_w CODE_FOR_lsx_vssran_s_h_w
#define CODE_FOR_lsx_vssran_w_d CODE_FOR_lsx_vssran_s_w_d
#define CODE_FOR_lsx_vssran_bu_h CODE_FOR_lsx_vssran_u_bu_h
#define CODE_FOR_lsx_vssran_hu_w CODE_FOR_lsx_vssran_u_hu_w
#define CODE_FOR_lsx_vssran_wu_d CODE_FOR_lsx_vssran_u_wu_d
#define CODE_FOR_lsx_vssrarn_b_h CODE_FOR_lsx_vssrarn_s_b_h
#define CODE_FOR_lsx_vssrarn_h_w CODE_FOR_lsx_vssrarn_s_h_w
#define CODE_FOR_lsx_vssrarn_w_d CODE_FOR_lsx_vssrarn_s_w_d
#define CODE_FOR_lsx_vssrarn_bu_h CODE_FOR_lsx_vssrarn_u_bu_h
#define CODE_FOR_lsx_vssrarn_hu_w CODE_FOR_lsx_vssrarn_u_hu_w
#define CODE_FOR_lsx_vssrarn_wu_d CODE_FOR_lsx_vssrarn_u_wu_d
#define CODE_FOR_lsx_vssrln_bu_h CODE_FOR_lsx_vssrln_u_bu_h
#define CODE_FOR_lsx_vssrln_hu_w CODE_FOR_lsx_vssrln_u_hu_w
#define CODE_FOR_lsx_vssrln_wu_d CODE_FOR_lsx_vssrln_u_wu_d
#define CODE_FOR_lsx_vssrlrn_bu_h CODE_FOR_lsx_vssrlrn_u_bu_h
#define CODE_FOR_lsx_vssrlrn_hu_w CODE_FOR_lsx_vssrlrn_u_hu_w
#define CODE_FOR_lsx_vssrlrn_wu_d CODE_FOR_lsx_vssrlrn_u_wu_d
#define CODE_FOR_lsx_vfrsqrt_d CODE_FOR_rsqrtv2df2
#define CODE_FOR_lsx_vfrsqrt_s CODE_FOR_rsqrtv4sf2
#define CODE_FOR_lsx_vfrecip_d CODE_FOR_recipv2df3
#define CODE_FOR_lsx_vfrecip_s CODE_FOR_recipv4sf3

#define CODE_FOR_lsx_vaddwev_q_d	CODE_FOR_lsx_addwev_q_d_punned
#define CODE_FOR_lsx_vaddwev_q_du	CODE_FOR_lsx_addwev_q_du_punned
#define CODE_FOR_lsx_vsubwev_q_d	CODE_FOR_lsx_subwev_q_d_punned
#define CODE_FOR_lsx_vsubwev_q_du	CODE_FOR_lsx_subwev_q_du_punned
#define CODE_FOR_lsx_vmulwev_q_d	CODE_FOR_lsx_mulwev_q_d_punned
#define CODE_FOR_lsx_vmulwev_q_du	CODE_FOR_lsx_mulwev_q_du_punned
#define CODE_FOR_lsx_vaddwod_q_d	CODE_FOR_lsx_addwod_q_d_punned
#define CODE_FOR_lsx_vaddwod_q_du	CODE_FOR_lsx_addwod_q_du_punned
#define CODE_FOR_lsx_vsubwod_q_d	CODE_FOR_lsx_subwod_q_d_punned
#define CODE_FOR_lsx_vsubwod_q_du	CODE_FOR_lsx_subwod_q_du_punned
#define CODE_FOR_lsx_vmulwod_q_d	CODE_FOR_lsx_mulwod_q_d_punned
#define CODE_FOR_lsx_vmulwod_q_du	CODE_FOR_lsx_mulwod_q_du_punned

#define CODE_FOR_lsx_vaddwev_q_du_d	CODE_FOR_lsx_addwev_q_du_d_punned
#define CODE_FOR_lsx_vmulwev_q_du_d	CODE_FOR_lsx_mulwev_q_du_d_punned
#define CODE_FOR_lsx_vaddwod_q_du_d	CODE_FOR_lsx_addwod_q_du_d_punned
#define CODE_FOR_lsx_vmulwod_q_du_d	CODE_FOR_lsx_mulwod_q_du_d_punned

#define CODE_FOR_lsx_vhaddw_q_d		CODE_FOR_lsx_haddw_q_d_punned
#define CODE_FOR_lsx_vhaddw_qu_du	CODE_FOR_lsx_haddw_qu_du_punned
#define CODE_FOR_lsx_vhsubw_q_d		CODE_FOR_lsx_hsubw_q_d_punned
#define CODE_FOR_lsx_vhsubw_qu_du	CODE_FOR_lsx_hsubw_qu_du_punned

#define CODE_FOR_lsx_vmaddwev_q_d	CODE_FOR_lsx_maddwev_q_d_punned
#define CODE_FOR_lsx_vmaddwev_q_du	CODE_FOR_lsx_maddwev_q_du_punned
#define CODE_FOR_lsx_vmaddwev_q_du_d	CODE_FOR_lsx_maddwev_q_du_d_punned
#define CODE_FOR_lsx_vmaddwod_q_d	CODE_FOR_lsx_maddwod_q_d_punned
#define CODE_FOR_lsx_vmaddwod_q_du	CODE_FOR_lsx_maddwod_q_du_punned
#define CODE_FOR_lsx_vmaddwod_q_du_d	CODE_FOR_lsx_maddwod_q_du_d_punned

/* LoongArch ASX define CODE_FOR_lasx_mxxx */
#define CODE_FOR_lasx_xvsadd_b CODE_FOR_ssaddv32qi3
#define CODE_FOR_lasx_xvsadd_h CODE_FOR_ssaddv16hi3
#define CODE_FOR_lasx_xvsadd_w CODE_FOR_ssaddv8si3
#define CODE_FOR_lasx_xvsadd_d CODE_FOR_ssaddv4di3
#define CODE_FOR_lasx_xvsadd_bu CODE_FOR_usaddv32qi3
#define CODE_FOR_lasx_xvsadd_hu CODE_FOR_usaddv16hi3
#define CODE_FOR_lasx_xvsadd_wu CODE_FOR_usaddv8si3
#define CODE_FOR_lasx_xvsadd_du CODE_FOR_usaddv4di3
#define CODE_FOR_lasx_xvadd_b CODE_FOR_addv32qi3
#define CODE_FOR_lasx_xvadd_h CODE_FOR_addv16hi3
#define CODE_FOR_lasx_xvadd_w CODE_FOR_addv8si3
#define CODE_FOR_lasx_xvadd_d CODE_FOR_addv4di3
#define CODE_FOR_lasx_xvaddi_bu CODE_FOR_addv32qi3
#define CODE_FOR_lasx_xvaddi_hu CODE_FOR_addv16hi3
#define CODE_FOR_lasx_xvaddi_wu CODE_FOR_addv8si3
#define CODE_FOR_lasx_xvaddi_du CODE_FOR_addv4di3
#define CODE_FOR_lasx_xvand_v CODE_FOR_andv32qi3
#define CODE_FOR_lasx_xvandi_b CODE_FOR_andv32qi3
#define CODE_FOR_lasx_xvbitsel_v CODE_FOR_lasx_xvbitsel_b
#define CODE_FOR_lasx_xvseqi_b CODE_FOR_lasx_xvseq_b
#define CODE_FOR_lasx_xvseqi_h CODE_FOR_lasx_xvseq_h
#define CODE_FOR_lasx_xvseqi_w CODE_FOR_lasx_xvseq_w
#define CODE_FOR_lasx_xvseqi_d CODE_FOR_lasx_xvseq_d
#define CODE_FOR_lasx_xvslti_b CODE_FOR_lasx_xvslt_b
#define CODE_FOR_lasx_xvslti_h CODE_FOR_lasx_xvslt_h
#define CODE_FOR_lasx_xvslti_w CODE_FOR_lasx_xvslt_w
#define CODE_FOR_lasx_xvslti_d CODE_FOR_lasx_xvslt_d
#define CODE_FOR_lasx_xvslti_bu CODE_FOR_lasx_xvslt_bu
#define CODE_FOR_lasx_xvslti_hu CODE_FOR_lasx_xvslt_hu
#define CODE_FOR_lasx_xvslti_wu CODE_FOR_lasx_xvslt_wu
#define CODE_FOR_lasx_xvslti_du CODE_FOR_lasx_xvslt_du
#define CODE_FOR_lasx_xvslei_b CODE_FOR_lasx_xvsle_b
#define CODE_FOR_lasx_xvslei_h CODE_FOR_lasx_xvsle_h
#define CODE_FOR_lasx_xvslei_w CODE_FOR_lasx_xvsle_w
#define CODE_FOR_lasx_xvslei_d CODE_FOR_lasx_xvsle_d
#define CODE_FOR_lasx_xvslei_bu CODE_FOR_lasx_xvsle_bu
#define CODE_FOR_lasx_xvslei_hu CODE_FOR_lasx_xvsle_hu
#define CODE_FOR_lasx_xvslei_wu CODE_FOR_lasx_xvsle_wu
#define CODE_FOR_lasx_xvslei_du CODE_FOR_lasx_xvsle_du
#define CODE_FOR_lasx_xvdiv_b CODE_FOR_divv32qi3
#define CODE_FOR_lasx_xvdiv_h CODE_FOR_divv16hi3
#define CODE_FOR_lasx_xvdiv_w CODE_FOR_divv8si3
#define CODE_FOR_lasx_xvdiv_d CODE_FOR_divv4di3
#define CODE_FOR_lasx_xvdiv_bu CODE_FOR_udivv32qi3
#define CODE_FOR_lasx_xvdiv_hu CODE_FOR_udivv16hi3
#define CODE_FOR_lasx_xvdiv_wu CODE_FOR_udivv8si3
#define CODE_FOR_lasx_xvdiv_du CODE_FOR_udivv4di3
#define CODE_FOR_lasx_xvfadd_s CODE_FOR_addv8sf3
#define CODE_FOR_lasx_xvfadd_d CODE_FOR_addv4df3
#define CODE_FOR_lasx_xvftintrz_w_s CODE_FOR_fix_truncv8sfv8si2
#define CODE_FOR_lasx_xvftintrz_l_d CODE_FOR_fix_truncv4dfv4di2
#define CODE_FOR_lasx_xvftintrz_wu_s CODE_FOR_fixuns_truncv8sfv8si2
#define CODE_FOR_lasx_xvftintrz_lu_d CODE_FOR_fixuns_truncv4dfv4di2
#define CODE_FOR_lasx_xvffint_s_w CODE_FOR_floatv8siv8sf2
#define CODE_FOR_lasx_xvffint_d_l CODE_FOR_floatv4div4df2
#define CODE_FOR_lasx_xvffint_s_wu CODE_FOR_floatunsv8siv8sf2
#define CODE_FOR_lasx_xvffint_d_lu CODE_FOR_floatunsv4div4df2
#define CODE_FOR_lasx_vext2xv_h_b CODE_FOR_vec_unpacks_lo_v32qi
#define CODE_FOR_lasx_vext2xv_w_h CODE_FOR_vec_unpacks_lo_v16hi
#define CODE_FOR_lasx_vext2xv_d_w CODE_FOR_vec_unpacks_lo_v8si
#define CODE_FOR_lasx_vext2xv_hu_bu CODE_FOR_vec_unpacku_lo_v32qi
#define CODE_FOR_lasx_vext2xv_wu_hu CODE_FOR_vec_unpacku_lo_v16hi
#define CODE_FOR_lasx_vext2xv_du_wu CODE_FOR_vec_unpacku_lo_v8si
#define CODE_FOR_lasx_xvfsub_s CODE_FOR_subv8sf3
#define CODE_FOR_lasx_xvfsub_d CODE_FOR_subv4df3
#define CODE_FOR_lasx_xvfmul_s CODE_FOR_mulv8sf3
#define CODE_FOR_lasx_xvfmul_d CODE_FOR_mulv4df3
#define CODE_FOR_lasx_xvfdiv_s CODE_FOR_divv8sf3
#define CODE_FOR_lasx_xvfdiv_d CODE_FOR_divv4df3
#define CODE_FOR_lasx_xvfmax_s CODE_FOR_smaxv8sf3
#define CODE_FOR_lasx_xvfmax_d CODE_FOR_smaxv4df3
#define CODE_FOR_lasx_xvfmin_s CODE_FOR_sminv8sf3
#define CODE_FOR_lasx_xvfmin_d CODE_FOR_sminv4df3
#define CODE_FOR_lasx_xvfsqrt_s CODE_FOR_sqrtv8sf2
#define CODE_FOR_lasx_xvfsqrt_d CODE_FOR_sqrtv4df2
#define CODE_FOR_lasx_xvflogb_s CODE_FOR_logbv8sf2
#define CODE_FOR_lasx_xvflogb_d CODE_FOR_logbv4df2
#define CODE_FOR_lasx_xvmax_b CODE_FOR_smaxv32qi3
#define CODE_FOR_lasx_xvmax_h CODE_FOR_smaxv16hi3
#define CODE_FOR_lasx_xvmax_w CODE_FOR_smaxv8si3
#define CODE_FOR_lasx_xvmax_d CODE_FOR_smaxv4di3
#define CODE_FOR_lasx_xvmaxi_b CODE_FOR_smaxv32qi3
#define CODE_FOR_lasx_xvmaxi_h CODE_FOR_smaxv16hi3
#define CODE_FOR_lasx_xvmaxi_w CODE_FOR_smaxv8si3
#define CODE_FOR_lasx_xvmaxi_d CODE_FOR_smaxv4di3
#define CODE_FOR_lasx_xvmax_bu CODE_FOR_umaxv32qi3
#define CODE_FOR_lasx_xvmax_hu CODE_FOR_umaxv16hi3
#define CODE_FOR_lasx_xvmax_wu CODE_FOR_umaxv8si3
#define CODE_FOR_lasx_xvmax_du CODE_FOR_umaxv4di3
#define CODE_FOR_lasx_xvmaxi_bu CODE_FOR_umaxv32qi3
#define CODE_FOR_lasx_xvmaxi_hu CODE_FOR_umaxv16hi3
#define CODE_FOR_lasx_xvmaxi_wu CODE_FOR_umaxv8si3
#define CODE_FOR_lasx_xvmaxi_du CODE_FOR_umaxv4di3
#define CODE_FOR_lasx_xvmin_b CODE_FOR_sminv32qi3
#define CODE_FOR_lasx_xvmin_h CODE_FOR_sminv16hi3
#define CODE_FOR_lasx_xvmin_w CODE_FOR_sminv8si3
#define CODE_FOR_lasx_xvmin_d CODE_FOR_sminv4di3
#define CODE_FOR_lasx_xvmini_b CODE_FOR_sminv32qi3
#define CODE_FOR_lasx_xvmini_h CODE_FOR_sminv16hi3
#define CODE_FOR_lasx_xvmini_w CODE_FOR_sminv8si3
#define CODE_FOR_lasx_xvmini_d CODE_FOR_sminv4di3
#define CODE_FOR_lasx_xvmin_bu CODE_FOR_uminv32qi3
#define CODE_FOR_lasx_xvmin_hu CODE_FOR_uminv16hi3
#define CODE_FOR_lasx_xvmin_wu CODE_FOR_uminv8si3
#define CODE_FOR_lasx_xvmin_du CODE_FOR_uminv4di3
#define CODE_FOR_lasx_xvmini_bu CODE_FOR_uminv32qi3
#define CODE_FOR_lasx_xvmini_hu CODE_FOR_uminv16hi3
#define CODE_FOR_lasx_xvmini_wu CODE_FOR_uminv8si3
#define CODE_FOR_lasx_xvmini_du CODE_FOR_uminv4di3
#define CODE_FOR_lasx_xvmod_b CODE_FOR_modv32qi3
#define CODE_FOR_lasx_xvmod_h CODE_FOR_modv16hi3
#define CODE_FOR_lasx_xvmod_w CODE_FOR_modv8si3
#define CODE_FOR_lasx_xvmod_d CODE_FOR_modv4di3
#define CODE_FOR_lasx_xvmod_bu CODE_FOR_umodv32qi3
#define CODE_FOR_lasx_xvmod_hu CODE_FOR_umodv16hi3
#define CODE_FOR_lasx_xvmod_wu CODE_FOR_umodv8si3
#define CODE_FOR_lasx_xvmod_du CODE_FOR_umodv4di3
#define CODE_FOR_lasx_xvmul_b CODE_FOR_mulv32qi3
#define CODE_FOR_lasx_xvmul_h CODE_FOR_mulv16hi3
#define CODE_FOR_lasx_xvmul_w CODE_FOR_mulv8si3
#define CODE_FOR_lasx_xvmul_d CODE_FOR_mulv4di3
#define CODE_FOR_lasx_xvmuh_b CODE_FOR_smulv32qi3_highpart
#define CODE_FOR_lasx_xvmuh_h CODE_FOR_smulv16hi3_highpart
#define CODE_FOR_lasx_xvmuh_w CODE_FOR_smulv8si3_highpart
#define CODE_FOR_lasx_xvmuh_d CODE_FOR_smulv4di3_highpart
#define CODE_FOR_lasx_xvmuh_bu CODE_FOR_umulv32qi3_highpart
#define CODE_FOR_lasx_xvmuh_hu CODE_FOR_umulv16hi3_highpart
#define CODE_FOR_lasx_xvmuh_wu CODE_FOR_umulv8si3_highpart
#define CODE_FOR_lasx_xvmuh_du CODE_FOR_umulv4di3_highpart
#define CODE_FOR_lasx_xvclz_b CODE_FOR_clzv32qi2
#define CODE_FOR_lasx_xvclz_h CODE_FOR_clzv16hi2
#define CODE_FOR_lasx_xvclz_w CODE_FOR_clzv8si2
#define CODE_FOR_lasx_xvclz_d CODE_FOR_clzv4di2
#define CODE_FOR_lasx_xvnor_v CODE_FOR_lasx_xvnor_b
#define CODE_FOR_lasx_xvor_v CODE_FOR_iorv32qi3
#define CODE_FOR_lasx_xvori_b CODE_FOR_iorv32qi3
#define CODE_FOR_lasx_xvnori_b CODE_FOR_lasx_xvnor_b
#define CODE_FOR_lasx_xvpcnt_b CODE_FOR_popcountv32qi2
#define CODE_FOR_lasx_xvpcnt_h CODE_FOR_popcountv16hi2
#define CODE_FOR_lasx_xvpcnt_w CODE_FOR_popcountv8si2
#define CODE_FOR_lasx_xvpcnt_d CODE_FOR_popcountv4di2
#define CODE_FOR_lasx_xvxor_v CODE_FOR_xorv32qi3
#define CODE_FOR_lasx_xvxori_b CODE_FOR_xorv32qi3
#define CODE_FOR_lasx_xvsll_b CODE_FOR_vashlv32qi3
#define CODE_FOR_lasx_xvsll_h CODE_FOR_vashlv16hi3
#define CODE_FOR_lasx_xvsll_w CODE_FOR_vashlv8si3
#define CODE_FOR_lasx_xvsll_d CODE_FOR_vashlv4di3
#define CODE_FOR_lasx_xvslli_b CODE_FOR_vashlv32qi3
#define CODE_FOR_lasx_xvslli_h CODE_FOR_vashlv16hi3
#define CODE_FOR_lasx_xvslli_w CODE_FOR_vashlv8si3
#define CODE_FOR_lasx_xvslli_d CODE_FOR_vashlv4di3
#define CODE_FOR_lasx_xvsra_b CODE_FOR_vashrv32qi3
#define CODE_FOR_lasx_xvsra_h CODE_FOR_vashrv16hi3
#define CODE_FOR_lasx_xvsra_w CODE_FOR_vashrv8si3
#define CODE_FOR_lasx_xvsra_d CODE_FOR_vashrv4di3
#define CODE_FOR_lasx_xvsrai_b CODE_FOR_vashrv32qi3
#define CODE_FOR_lasx_xvsrai_h CODE_FOR_vashrv16hi3
#define CODE_FOR_lasx_xvsrai_w CODE_FOR_vashrv8si3
#define CODE_FOR_lasx_xvsrai_d CODE_FOR_vashrv4di3
#define CODE_FOR_lasx_xvsrl_b CODE_FOR_vlshrv32qi3
#define CODE_FOR_lasx_xvsrl_h CODE_FOR_vlshrv16hi3
#define CODE_FOR_lasx_xvsrl_w CODE_FOR_vlshrv8si3
#define CODE_FOR_lasx_xvsrl_d CODE_FOR_vlshrv4di3
#define CODE_FOR_lasx_xvsrli_b CODE_FOR_vlshrv32qi3
#define CODE_FOR_lasx_xvsrli_h CODE_FOR_vlshrv16hi3
#define CODE_FOR_lasx_xvsrli_w CODE_FOR_vlshrv8si3
#define CODE_FOR_lasx_xvsrli_d CODE_FOR_vlshrv4di3
#define CODE_FOR_lasx_xvrotr_b CODE_FOR_vrotrv32qi3
#define CODE_FOR_lasx_xvrotr_h CODE_FOR_vrotrv16hi3
#define CODE_FOR_lasx_xvrotr_w CODE_FOR_vrotrv8si3
#define CODE_FOR_lasx_xvrotr_d CODE_FOR_vrotrv4di3
#define CODE_FOR_lasx_xvrotri_b CODE_FOR_rotrv32qi3
#define CODE_FOR_lasx_xvrotri_h CODE_FOR_rotrv16hi3
#define CODE_FOR_lasx_xvrotri_w CODE_FOR_rotrv8si3
#define CODE_FOR_lasx_xvrotri_d CODE_FOR_rotrv4di3
#define CODE_FOR_lasx_xvsub_b CODE_FOR_subv32qi3
#define CODE_FOR_lasx_xvsub_h CODE_FOR_subv16hi3
#define CODE_FOR_lasx_xvsub_w CODE_FOR_subv8si3
#define CODE_FOR_lasx_xvsub_d CODE_FOR_subv4di3
#define CODE_FOR_lasx_xvsubi_bu CODE_FOR_subv32qi3
#define CODE_FOR_lasx_xvsubi_hu CODE_FOR_subv16hi3
#define CODE_FOR_lasx_xvsubi_wu CODE_FOR_subv8si3
#define CODE_FOR_lasx_xvsubi_du CODE_FOR_subv4di3
#define CODE_FOR_lasx_xvpackod_d CODE_FOR_lasx_xvilvh_d
#define CODE_FOR_lasx_xvpackev_d CODE_FOR_lasx_xvilvl_d
#define CODE_FOR_lasx_xvpickod_d CODE_FOR_lasx_xvilvh_d
#define CODE_FOR_lasx_xvpickev_d CODE_FOR_lasx_xvilvl_d
#define CODE_FOR_lasx_xvrepli_b CODE_FOR_lasx_xvrepliv32qi
#define CODE_FOR_lasx_xvrepli_h CODE_FOR_lasx_xvrepliv16hi
#define CODE_FOR_lasx_xvrepli_w CODE_FOR_lasx_xvrepliv8si
#define CODE_FOR_lasx_xvrepli_d CODE_FOR_lasx_xvrepliv4di

#define CODE_FOR_lasx_xvandn_v CODE_FOR_andnv32qi3
#define CODE_FOR_lasx_xvorn_v CODE_FOR_iornv32qi3
#define CODE_FOR_lasx_xvneg_b CODE_FOR_negv32qi2
#define CODE_FOR_lasx_xvneg_h CODE_FOR_negv16hi2
#define CODE_FOR_lasx_xvneg_w CODE_FOR_negv8si2
#define CODE_FOR_lasx_xvneg_d CODE_FOR_negv4di2
#define CODE_FOR_lasx_xvbsrl_v CODE_FOR_lasx_xvbsrl_b
#define CODE_FOR_lasx_xvbsll_v CODE_FOR_lasx_xvbsll_b
#define CODE_FOR_lasx_xvfmadd_s CODE_FOR_fmav8sf4
#define CODE_FOR_lasx_xvfmadd_d CODE_FOR_fmav4df4
#define CODE_FOR_lasx_xvfmsub_s CODE_FOR_fmsv8sf4
#define CODE_FOR_lasx_xvfmsub_d CODE_FOR_fmsv4df4
#define CODE_FOR_lasx_xvfnmadd_s CODE_FOR_xvfnmaddv8sf4_nmadd4
#define CODE_FOR_lasx_xvfnmadd_d CODE_FOR_xvfnmaddv4df4_nmadd4
#define CODE_FOR_lasx_xvfnmsub_s CODE_FOR_xvfnmsubv8sf4_nmsub4
#define CODE_FOR_lasx_xvfnmsub_d CODE_FOR_xvfnmsubv4df4_nmsub4

#define CODE_FOR_lasx_xvpermi_q CODE_FOR_lasx_xvpermi_q_v32qi
#define CODE_FOR_lasx_xvpermi_d CODE_FOR_lasx_xvpermi_d_v4di
#define CODE_FOR_lasx_xbnz_v CODE_FOR_lasx_xbnz_v_b
#define CODE_FOR_lasx_xbz_v CODE_FOR_lasx_xbz_v_b

#define CODE_FOR_lasx_xvssub_b CODE_FOR_lasx_xvssub_s_b
#define CODE_FOR_lasx_xvssub_h CODE_FOR_lasx_xvssub_s_h
#define CODE_FOR_lasx_xvssub_w CODE_FOR_lasx_xvssub_s_w
#define CODE_FOR_lasx_xvssub_d CODE_FOR_lasx_xvssub_s_d
#define CODE_FOR_lasx_xvssub_bu CODE_FOR_lasx_xvssub_u_bu
#define CODE_FOR_lasx_xvssub_hu CODE_FOR_lasx_xvssub_u_hu
#define CODE_FOR_lasx_xvssub_wu CODE_FOR_lasx_xvssub_u_wu
#define CODE_FOR_lasx_xvssub_du CODE_FOR_lasx_xvssub_u_du
#define CODE_FOR_lasx_xvabsd_b CODE_FOR_sabdv32qi3
#define CODE_FOR_lasx_xvabsd_h CODE_FOR_sabdv16hi3
#define CODE_FOR_lasx_xvabsd_w CODE_FOR_sabdv8si3
#define CODE_FOR_lasx_xvabsd_d CODE_FOR_sabdv4di3
#define CODE_FOR_lasx_xvabsd_bu CODE_FOR_uabdv32qi3
#define CODE_FOR_lasx_xvabsd_hu CODE_FOR_uabdv16hi3
#define CODE_FOR_lasx_xvabsd_wu CODE_FOR_uabdv8si3
#define CODE_FOR_lasx_xvabsd_du CODE_FOR_uabdv4di3
#define CODE_FOR_lasx_xvavg_b CODE_FOR_lasx_xvavg_s_b
#define CODE_FOR_lasx_xvavg_h CODE_FOR_lasx_xvavg_s_h
#define CODE_FOR_lasx_xvavg_w CODE_FOR_lasx_xvavg_s_w
#define CODE_FOR_lasx_xvavg_d CODE_FOR_lasx_xvavg_s_d
#define CODE_FOR_lasx_xvavg_bu CODE_FOR_lasx_xvavg_u_bu
#define CODE_FOR_lasx_xvavg_hu CODE_FOR_lasx_xvavg_u_hu
#define CODE_FOR_lasx_xvavg_wu CODE_FOR_lasx_xvavg_u_wu
#define CODE_FOR_lasx_xvavg_du CODE_FOR_lasx_xvavg_u_du
#define CODE_FOR_lasx_xvavgr_b CODE_FOR_lasx_xvavgr_s_b
#define CODE_FOR_lasx_xvavgr_h CODE_FOR_lasx_xvavgr_s_h
#define CODE_FOR_lasx_xvavgr_w CODE_FOR_lasx_xvavgr_s_w
#define CODE_FOR_lasx_xvavgr_d CODE_FOR_lasx_xvavgr_s_d
#define CODE_FOR_lasx_xvavgr_bu CODE_FOR_lasx_xvavgr_u_bu
#define CODE_FOR_lasx_xvavgr_hu CODE_FOR_lasx_xvavgr_u_hu
#define CODE_FOR_lasx_xvavgr_wu CODE_FOR_lasx_xvavgr_u_wu
#define CODE_FOR_lasx_xvavgr_du CODE_FOR_lasx_xvavgr_u_du
#define CODE_FOR_lasx_xvssran_b_h CODE_FOR_lasx_xvssran_s_b_h
#define CODE_FOR_lasx_xvssran_h_w CODE_FOR_lasx_xvssran_s_h_w
#define CODE_FOR_lasx_xvssran_w_d CODE_FOR_lasx_xvssran_s_w_d
#define CODE_FOR_lasx_xvssran_bu_h CODE_FOR_lasx_xvssran_u_bu_h
#define CODE_FOR_lasx_xvssran_hu_w CODE_FOR_lasx_xvssran_u_hu_w
#define CODE_FOR_lasx_xvssran_wu_d CODE_FOR_lasx_xvssran_u_wu_d
#define CODE_FOR_lasx_xvssrarn_b_h CODE_FOR_lasx_xvssrarn_s_b_h
#define CODE_FOR_lasx_xvssrarn_h_w CODE_FOR_lasx_xvssrarn_s_h_w
#define CODE_FOR_lasx_xvssrarn_w_d CODE_FOR_lasx_xvssrarn_s_w_d
#define CODE_FOR_lasx_xvssrarn_bu_h CODE_FOR_lasx_xvssrarn_u_bu_h
#define CODE_FOR_lasx_xvssrarn_hu_w CODE_FOR_lasx_xvssrarn_u_hu_w
#define CODE_FOR_lasx_xvssrarn_wu_d CODE_FOR_lasx_xvssrarn_u_wu_d
#define CODE_FOR_lasx_xvssrln_bu_h CODE_FOR_lasx_xvssrln_u_bu_h
#define CODE_FOR_lasx_xvssrln_hu_w CODE_FOR_lasx_xvssrln_u_hu_w
#define CODE_FOR_lasx_xvssrln_wu_d CODE_FOR_lasx_xvssrln_u_wu_d
#define CODE_FOR_lasx_xvssrlrn_bu_h CODE_FOR_lasx_xvssrlrn_u_bu_h
#define CODE_FOR_lasx_xvssrlrn_hu_w CODE_FOR_lasx_xvssrlrn_u_hu_w
#define CODE_FOR_lasx_xvssrlrn_wu_d CODE_FOR_lasx_xvssrlrn_u_wu_d
#define CODE_FOR_lasx_xvftint_wu_s CODE_FOR_lasx_xvftint_u_wu_s
#define CODE_FOR_lasx_xvftint_lu_d CODE_FOR_lasx_xvftint_u_lu_d
#define CODE_FOR_lasx_xvsllwil_h_b CODE_FOR_lasx_xvsllwil_s_h_b
#define CODE_FOR_lasx_xvsllwil_w_h CODE_FOR_lasx_xvsllwil_s_w_h
#define CODE_FOR_lasx_xvsllwil_d_w CODE_FOR_lasx_xvsllwil_s_d_w
#define CODE_FOR_lasx_xvsllwil_hu_bu CODE_FOR_lasx_xvsllwil_u_hu_bu
#define CODE_FOR_lasx_xvsllwil_wu_hu CODE_FOR_lasx_xvsllwil_u_wu_hu
#define CODE_FOR_lasx_xvsllwil_du_wu CODE_FOR_lasx_xvsllwil_u_du_wu
#define CODE_FOR_lasx_xvsat_b CODE_FOR_lasx_xvsat_s_b
#define CODE_FOR_lasx_xvsat_h CODE_FOR_lasx_xvsat_s_h
#define CODE_FOR_lasx_xvsat_w CODE_FOR_lasx_xvsat_s_w
#define CODE_FOR_lasx_xvsat_d CODE_FOR_lasx_xvsat_s_d
#define CODE_FOR_lasx_xvsat_bu CODE_FOR_lasx_xvsat_u_bu
#define CODE_FOR_lasx_xvsat_hu CODE_FOR_lasx_xvsat_u_hu
#define CODE_FOR_lasx_xvsat_wu CODE_FOR_lasx_xvsat_u_wu
#define CODE_FOR_lasx_xvsat_du CODE_FOR_lasx_xvsat_u_du
#define CODE_FOR_lasx_xvfrsqrt_d CODE_FOR_rsqrtv4df2
#define CODE_FOR_lasx_xvfrsqrt_s CODE_FOR_rsqrtv8sf2
#define CODE_FOR_lasx_xvfrecip_d CODE_FOR_recipv4df3
#define CODE_FOR_lasx_xvfrecip_s CODE_FOR_recipv8sf3

#define CODE_FOR_lasx_xvaddwev_q_d	CODE_FOR_lasx_addwev_q_d_punned
#define CODE_FOR_lasx_xvaddwev_q_du	CODE_FOR_lasx_addwev_q_du_punned
#define CODE_FOR_lasx_xvsubwev_q_d	CODE_FOR_lasx_subwev_q_d_punned
#define CODE_FOR_lasx_xvsubwev_q_du	CODE_FOR_lasx_subwev_q_du_punned
#define CODE_FOR_lasx_xvmulwev_q_d	CODE_FOR_lasx_mulwev_q_d_punned
#define CODE_FOR_lasx_xvmulwev_q_du	CODE_FOR_lasx_mulwev_q_du_punned
#define CODE_FOR_lasx_xvaddwod_q_d	CODE_FOR_lasx_addwod_q_d_punned
#define CODE_FOR_lasx_xvaddwod_q_du	CODE_FOR_lasx_addwod_q_du_punned
#define CODE_FOR_lasx_xvsubwod_q_d	CODE_FOR_lasx_subwod_q_d_punned
#define CODE_FOR_lasx_xvsubwod_q_du	CODE_FOR_lasx_subwod_q_du_punned
#define CODE_FOR_lasx_xvmulwod_q_d	CODE_FOR_lasx_mulwod_q_d_punned
#define CODE_FOR_lasx_xvmulwod_q_du	CODE_FOR_lasx_mulwod_q_du_punned

#define CODE_FOR_lasx_xvaddwev_q_du_d	CODE_FOR_lasx_addwev_q_du_d_punned
#define CODE_FOR_lasx_xvmulwev_q_du_d	CODE_FOR_lasx_mulwev_q_du_d_punned
#define CODE_FOR_lasx_xvaddwod_q_du_d	CODE_FOR_lasx_addwod_q_du_d_punned
#define CODE_FOR_lasx_xvmulwod_q_du_d	CODE_FOR_lasx_mulwod_q_du_d_punned

#define CODE_FOR_lasx_xvhaddw_q_d	CODE_FOR_lasx_haddw_q_d_punned
#define CODE_FOR_lasx_xvhaddw_qu_du	CODE_FOR_lasx_haddw_qu_du_punned
#define CODE_FOR_lasx_xvhsubw_q_d	CODE_FOR_lasx_hsubw_q_d_punned
#define CODE_FOR_lasx_xvhsubw_qu_du	CODE_FOR_lasx_hsubw_qu_du_punned

#define CODE_FOR_lasx_xvmaddwev_q_d	CODE_FOR_lasx_maddwev_q_d_punned
#define CODE_FOR_lasx_xvmaddwev_q_du	CODE_FOR_lasx_maddwev_q_du_punned
#define CODE_FOR_lasx_xvmaddwev_q_du_d	CODE_FOR_lasx_maddwev_q_du_d_punned
#define CODE_FOR_lasx_xvmaddwod_q_d	CODE_FOR_lasx_maddwod_q_d_punned
#define CODE_FOR_lasx_xvmaddwod_q_du	CODE_FOR_lasx_maddwod_q_du_punned
#define CODE_FOR_lasx_xvmaddwod_q_du_d	CODE_FOR_lasx_maddwod_q_du_d_punned

static const struct loongarch_builtin_description loongarch_builtins[] = {
#define LARCH_MOVFCSR2GR 0
  DIRECT_BUILTIN (movfcsr2gr, LARCH_USI_FTYPE_UQI, hard_float),
#define LARCH_MOVGR2FCSR 1
  DIRECT_NO_TARGET_BUILTIN (movgr2fcsr, LARCH_VOID_FTYPE_UQI_USI, hard_float),

  DIRECT_NO_TARGET_BUILTIN (cacop_w, LARCH_VOID_FTYPE_USI_USI_SI, default),
  DIRECT_NO_TARGET_BUILTIN (cacop_d, LARCH_VOID_FTYPE_USI_UDI_SI, default),
  DIRECT_NO_TARGET_BUILTIN (dbar, LARCH_VOID_FTYPE_USI, default),
  DIRECT_NO_TARGET_BUILTIN (ibar, LARCH_VOID_FTYPE_USI, default),

  DIRECT_BUILTIN (lddir_d, LARCH_DI_FTYPE_DI_UQI, default),
  DIRECT_BUILTIN (lddir_w, LARCH_SI_FTYPE_SI_UQI, default),
  DIRECT_NO_TARGET_BUILTIN (ldpte_d, LARCH_VOID_FTYPE_DI_UQI, default),
  DIRECT_NO_TARGET_BUILTIN (ldpte_w, LARCH_VOID_FTYPE_SI_UQI, default),

  /* CRC Instrinsic */

  DIRECT_BUILTIN (crc_w_b_w, LARCH_SI_FTYPE_QI_SI, default),
  DIRECT_BUILTIN (crc_w_h_w, LARCH_SI_FTYPE_HI_SI, default),
  DIRECT_BUILTIN (crc_w_w_w, LARCH_SI_FTYPE_SI_SI, default),
  DIRECT_BUILTIN (crc_w_d_w, LARCH_SI_FTYPE_DI_SI, default),
  DIRECT_BUILTIN (crcc_w_b_w, LARCH_SI_FTYPE_QI_SI, default),
  DIRECT_BUILTIN (crcc_w_h_w, LARCH_SI_FTYPE_HI_SI, default),
  DIRECT_BUILTIN (crcc_w_w_w, LARCH_SI_FTYPE_SI_SI, default),
  DIRECT_BUILTIN (crcc_w_d_w, LARCH_SI_FTYPE_DI_SI, default),

  DIRECT_BUILTIN (csrrd_w, LARCH_USI_FTYPE_USI, default),
  DIRECT_BUILTIN (csrrd_d, LARCH_UDI_FTYPE_USI, default),
  DIRECT_BUILTIN (csrwr_w, LARCH_USI_FTYPE_USI_USI, default),
  DIRECT_BUILTIN (csrwr_d, LARCH_UDI_FTYPE_UDI_USI, default),
  DIRECT_BUILTIN (csrxchg_w, LARCH_USI_FTYPE_USI_USI_USI, default),
  DIRECT_BUILTIN (csrxchg_d, LARCH_UDI_FTYPE_UDI_UDI_USI, default),
  DIRECT_BUILTIN (iocsrrd_b, LARCH_UQI_FTYPE_USI, default),
  DIRECT_BUILTIN (iocsrrd_h, LARCH_UHI_FTYPE_USI, default),
  DIRECT_BUILTIN (iocsrrd_w, LARCH_USI_FTYPE_USI, default),
  DIRECT_BUILTIN (iocsrrd_d, LARCH_UDI_FTYPE_USI, default),
  DIRECT_NO_TARGET_BUILTIN (iocsrwr_b, LARCH_VOID_FTYPE_UQI_USI, default),
  DIRECT_NO_TARGET_BUILTIN (iocsrwr_h, LARCH_VOID_FTYPE_UHI_USI, default),
  DIRECT_NO_TARGET_BUILTIN (iocsrwr_w, LARCH_VOID_FTYPE_USI_USI, default),
  DIRECT_NO_TARGET_BUILTIN (iocsrwr_d, LARCH_VOID_FTYPE_UDI_USI, default),

  DIRECT_BUILTIN (cpucfg, LARCH_USI_FTYPE_USI, default),
  DIRECT_NO_TARGET_BUILTIN (asrtle_d, LARCH_VOID_FTYPE_DI_DI, default),
  DIRECT_NO_TARGET_BUILTIN (asrtgt_d, LARCH_VOID_FTYPE_DI_DI, default),
  DIRECT_NO_TARGET_BUILTIN (syscall, LARCH_VOID_FTYPE_USI, default),
  DIRECT_NO_TARGET_BUILTIN (break, LARCH_VOID_FTYPE_USI, default),

  /* Built-in functions for frecipe.{s/d} and frsqrte.{s/d}.  */

  DIRECT_BUILTIN (frecipe_s, LARCH_SF_FTYPE_SF, frecipe),
  DIRECT_BUILTIN (frecipe_d, LARCH_DF_FTYPE_DF, frecipe),
  DIRECT_BUILTIN (frsqrte_s, LARCH_SF_FTYPE_SF, frecipe),
  DIRECT_BUILTIN (frsqrte_d, LARCH_DF_FTYPE_DF, frecipe),

  /* Built-in functions for new LSX instructions.  */

  LSX_EXT_BUILTIN (vfrecipe_s, LARCH_V4SF_FTYPE_V4SF, lsx_frecipe),
  LSX_EXT_BUILTIN (vfrecipe_d, LARCH_V2DF_FTYPE_V2DF, lsx_frecipe),
  LSX_EXT_BUILTIN (vfrsqrte_s, LARCH_V4SF_FTYPE_V4SF, lsx_frecipe),
  LSX_EXT_BUILTIN (vfrsqrte_d, LARCH_V2DF_FTYPE_V2DF, lsx_frecipe),

  /* Built-in functions for new LASX instructions.  */

  LASX_EXT_BUILTIN (xvfrecipe_s, LARCH_V8SF_FTYPE_V8SF, lasx_frecipe),
  LASX_EXT_BUILTIN (xvfrecipe_d, LARCH_V4DF_FTYPE_V4DF, lasx_frecipe),
  LASX_EXT_BUILTIN (xvfrsqrte_s, LARCH_V8SF_FTYPE_V8SF, lasx_frecipe),
  LASX_EXT_BUILTIN (xvfrsqrte_d, LARCH_V4DF_FTYPE_V4DF, lasx_frecipe),

  /* Built-in functions for LSX.  */
  LSX_BUILTIN (vsll_b, LARCH_V16QI_FTYPE_V16QI_V16QI),
  LSX_BUILTIN (vsll_h, LARCH_V8HI_FTYPE_V8HI_V8HI),
  LSX_BUILTIN (vsll_w, LARCH_V4SI_FTYPE_V4SI_V4SI),
  LSX_BUILTIN (vsll_d, LARCH_V2DI_FTYPE_V2DI_V2DI),
  LSX_BUILTIN (vslli_b, LARCH_V16QI_FTYPE_V16QI_UQI),
  LSX_BUILTIN (vslli_h, LARCH_V8HI_FTYPE_V8HI_UQI),
  LSX_BUILTIN (vslli_w, LARCH_V4SI_FTYPE_V4SI_UQI),
  LSX_BUILTIN (vslli_d, LARCH_V2DI_FTYPE_V2DI_UQI),
  LSX_BUILTIN (vsra_b, LARCH_V16QI_FTYPE_V16QI_V16QI),
  LSX_BUILTIN (vsra_h, LARCH_V8HI_FTYPE_V8HI_V8HI),
  LSX_BUILTIN (vsra_w, LARCH_V4SI_FTYPE_V4SI_V4SI),
  LSX_BUILTIN (vsra_d, LARCH_V2DI_FTYPE_V2DI_V2DI),
  LSX_BUILTIN (vsrai_b, LARCH_V16QI_FTYPE_V16QI_UQI),
  LSX_BUILTIN (vsrai_h, LARCH_V8HI_FTYPE_V8HI_UQI),
  LSX_BUILTIN (vsrai_w, LARCH_V4SI_FTYPE_V4SI_UQI),
  LSX_BUILTIN (vsrai_d, LARCH_V2DI_FTYPE_V2DI_UQI),
  LSX_BUILTIN (vsrar_b, LARCH_V16QI_FTYPE_V16QI_V16QI),
  LSX_BUILTIN (vsrar_h, LARCH_V8HI_FTYPE_V8HI_V8HI),
  LSX_BUILTIN (vsrar_w, LARCH_V4SI_FTYPE_V4SI_V4SI),
  LSX_BUILTIN (vsrar_d, LARCH_V2DI_FTYPE_V2DI_V2DI),
  LSX_BUILTIN (vsrari_b, LARCH_V16QI_FTYPE_V16QI_UQI),
  LSX_BUILTIN (vsrari_h, LARCH_V8HI_FTYPE_V8HI_UQI),
  LSX_BUILTIN (vsrari_w, LARCH_V4SI_FTYPE_V4SI_UQI),
  LSX_BUILTIN (vsrari_d, LARCH_V2DI_FTYPE_V2DI_UQI),
  LSX_BUILTIN (vsrl_b, LARCH_V16QI_FTYPE_V16QI_V16QI),
  LSX_BUILTIN (vsrl_h, LARCH_V8HI_FTYPE_V8HI_V8HI),
  LSX_BUILTIN (vsrl_w, LARCH_V4SI_FTYPE_V4SI_V4SI),
  LSX_BUILTIN (vsrl_d, LARCH_V2DI_FTYPE_V2DI_V2DI),
  LSX_BUILTIN (vsrli_b, LARCH_V16QI_FTYPE_V16QI_UQI),
  LSX_BUILTIN (vsrli_h, LARCH_V8HI_FTYPE_V8HI_UQI),
  LSX_BUILTIN (vsrli_w, LARCH_V4SI_FTYPE_V4SI_UQI),
  LSX_BUILTIN (vsrli_d, LARCH_V2DI_FTYPE_V2DI_UQI),
  LSX_BUILTIN (vsrlr_b, LARCH_V16QI_FTYPE_V16QI_V16QI),
  LSX_BUILTIN (vsrlr_h, LARCH_V8HI_FTYPE_V8HI_V8HI),
  LSX_BUILTIN (vsrlr_w, LARCH_V4SI_FTYPE_V4SI_V4SI),
  LSX_BUILTIN (vsrlr_d, LARCH_V2DI_FTYPE_V2DI_V2DI),
  LSX_BUILTIN (vsrlri_b, LARCH_V16QI_FTYPE_V16QI_UQI),
  LSX_BUILTIN (vsrlri_h, LARCH_V8HI_FTYPE_V8HI_UQI),
  LSX_BUILTIN (vsrlri_w, LARCH_V4SI_FTYPE_V4SI_UQI),
  LSX_BUILTIN (vsrlri_d, LARCH_V2DI_FTYPE_V2DI_UQI),
  LSX_BUILTIN (vbitclr_b, LARCH_UV16QI_FTYPE_UV16QI_UV16QI),
  LSX_BUILTIN (vbitclr_h, LARCH_UV8HI_FTYPE_UV8HI_UV8HI),
  LSX_BUILTIN (vbitclr_w, LARCH_UV4SI_FTYPE_UV4SI_UV4SI),
  LSX_BUILTIN (vbitclr_d, LARCH_UV2DI_FTYPE_UV2DI_UV2DI),
  LSX_BUILTIN (vbitclri_b, LARCH_UV16QI_FTYPE_UV16QI_UQI),
  LSX_BUILTIN (vbitclri_h, LARCH_UV8HI_FTYPE_UV8HI_UQI),
  LSX_BUILTIN (vbitclri_w, LARCH_UV4SI_FTYPE_UV4SI_UQI),
  LSX_BUILTIN (vbitclri_d, LARCH_UV2DI_FTYPE_UV2DI_UQI),
  LSX_BUILTIN (vbitset_b, LARCH_UV16QI_FTYPE_UV16QI_UV16QI),
  LSX_BUILTIN (vbitset_h, LARCH_UV8HI_FTYPE_UV8HI_UV8HI),
  LSX_BUILTIN (vbitset_w, LARCH_UV4SI_FTYPE_UV4SI_UV4SI),
  LSX_BUILTIN (vbitset_d, LARCH_UV2DI_FTYPE_UV2DI_UV2DI),
  LSX_BUILTIN (vbitseti_b, LARCH_UV16QI_FTYPE_UV16QI_UQI),
  LSX_BUILTIN (vbitseti_h, LARCH_UV8HI_FTYPE_UV8HI_UQI),
  LSX_BUILTIN (vbitseti_w, LARCH_UV4SI_FTYPE_UV4SI_UQI),
  LSX_BUILTIN (vbitseti_d, LARCH_UV2DI_FTYPE_UV2DI_UQI),
  LSX_BUILTIN (vbitrev_b, LARCH_UV16QI_FTYPE_UV16QI_UV16QI),
  LSX_BUILTIN (vbitrev_h, LARCH_UV8HI_FTYPE_UV8HI_UV8HI),
  LSX_BUILTIN (vbitrev_w, LARCH_UV4SI_FTYPE_UV4SI_UV4SI),
  LSX_BUILTIN (vbitrev_d, LARCH_UV2DI_FTYPE_UV2DI_UV2DI),
  LSX_BUILTIN (vbitrevi_b, LARCH_UV16QI_FTYPE_UV16QI_UQI),
  LSX_BUILTIN (vbitrevi_h, LARCH_UV8HI_FTYPE_UV8HI_UQI),
  LSX_BUILTIN (vbitrevi_w, LARCH_UV4SI_FTYPE_UV4SI_UQI),
  LSX_BUILTIN (vbitrevi_d, LARCH_UV2DI_FTYPE_UV2DI_UQI),
  LSX_BUILTIN (vadd_b, LARCH_V16QI_FTYPE_V16QI_V16QI),
  LSX_BUILTIN (vadd_h, LARCH_V8HI_FTYPE_V8HI_V8HI),
  LSX_BUILTIN (vadd_w, LARCH_V4SI_FTYPE_V4SI_V4SI),
  LSX_BUILTIN (vadd_d, LARCH_V2DI_FTYPE_V2DI_V2DI),
  LSX_BUILTIN (vaddi_bu, LARCH_V16QI_FTYPE_V16QI_UQI),
  LSX_BUILTIN (vaddi_hu, LARCH_V8HI_FTYPE_V8HI_UQI),
  LSX_BUILTIN (vaddi_wu, LARCH_V4SI_FTYPE_V4SI_UQI),
  LSX_BUILTIN (vaddi_du, LARCH_V2DI_FTYPE_V2DI_UQI),
  LSX_BUILTIN (vsub_b, LARCH_V16QI_FTYPE_V16QI_V16QI),
  LSX_BUILTIN (vsub_h, LARCH_V8HI_FTYPE_V8HI_V8HI),
  LSX_BUILTIN (vsub_w, LARCH_V4SI_FTYPE_V4SI_V4SI),
  LSX_BUILTIN (vsub_d, LARCH_V2DI_FTYPE_V2DI_V2DI),
  LSX_BUILTIN (vsubi_bu, LARCH_V16QI_FTYPE_V16QI_UQI),
  LSX_BUILTIN (vsubi_hu, LARCH_V8HI_FTYPE_V8HI_UQI),
  LSX_BUILTIN (vsubi_wu, LARCH_V4SI_FTYPE_V4SI_UQI),
  LSX_BUILTIN (vsubi_du, LARCH_V2DI_FTYPE_V2DI_UQI),
  LSX_BUILTIN (vmax_b, LARCH_V16QI_FTYPE_V16QI_V16QI),
  LSX_BUILTIN (vmax_h, LARCH_V8HI_FTYPE_V8HI_V8HI),
  LSX_BUILTIN (vmax_w, LARCH_V4SI_FTYPE_V4SI_V4SI),
  LSX_BUILTIN (vmax_d, LARCH_V2DI_FTYPE_V2DI_V2DI),
  LSX_BUILTIN (vmaxi_b, LARCH_V16QI_FTYPE_V16QI_QI),
  LSX_BUILTIN (vmaxi_h, LARCH_V8HI_FTYPE_V8HI_QI),
  LSX_BUILTIN (vmaxi_w, LARCH_V4SI_FTYPE_V4SI_QI),
  LSX_BUILTIN (vmaxi_d, LARCH_V2DI_FTYPE_V2DI_QI),
  LSX_BUILTIN (vmax_bu, LARCH_UV16QI_FTYPE_UV16QI_UV16QI),
  LSX_BUILTIN (vmax_hu, LARCH_UV8HI_FTYPE_UV8HI_UV8HI),
  LSX_BUILTIN (vmax_wu, LARCH_UV4SI_FTYPE_UV4SI_UV4SI),
  LSX_BUILTIN (vmax_du, LARCH_UV2DI_FTYPE_UV2DI_UV2DI),
  LSX_BUILTIN (vmaxi_bu, LARCH_UV16QI_FTYPE_UV16QI_UQI),
  LSX_BUILTIN (vmaxi_hu, LARCH_UV8HI_FTYPE_UV8HI_UQI),
  LSX_BUILTIN (vmaxi_wu, LARCH_UV4SI_FTYPE_UV4SI_UQI),
  LSX_BUILTIN (vmaxi_du, LARCH_UV2DI_FTYPE_UV2DI_UQI),
  LSX_BUILTIN (vmin_b, LARCH_V16QI_FTYPE_V16QI_V16QI),
  LSX_BUILTIN (vmin_h, LARCH_V8HI_FTYPE_V8HI_V8HI),
  LSX_BUILTIN (vmin_w, LARCH_V4SI_FTYPE_V4SI_V4SI),
  LSX_BUILTIN (vmin_d, LARCH_V2DI_FTYPE_V2DI_V2DI),
  LSX_BUILTIN (vmini_b, LARCH_V16QI_FTYPE_V16QI_QI),
  LSX_BUILTIN (vmini_h, LARCH_V8HI_FTYPE_V8HI_QI),
  LSX_BUILTIN (vmini_w, LARCH_V4SI_FTYPE_V4SI_QI),
  LSX_BUILTIN (vmini_d, LARCH_V2DI_FTYPE_V2DI_QI),
  LSX_BUILTIN (vmin_bu, LARCH_UV16QI_FTYPE_UV16QI_UV16QI),
  LSX_BUILTIN (vmin_hu, LARCH_UV8HI_FTYPE_UV8HI_UV8HI),
  LSX_BUILTIN (vmin_wu, LARCH_UV4SI_FTYPE_UV4SI_UV4SI),
  LSX_BUILTIN (vmin_du, LARCH_UV2DI_FTYPE_UV2DI_UV2DI),
  LSX_BUILTIN (vmini_bu, LARCH_UV16QI_FTYPE_UV16QI_UQI),
  LSX_BUILTIN (vmini_hu, LARCH_UV8HI_FTYPE_UV8HI_UQI),
  LSX_BUILTIN (vmini_wu, LARCH_UV4SI_FTYPE_UV4SI_UQI),
  LSX_BUILTIN (vmini_du, LARCH_UV2DI_FTYPE_UV2DI_UQI),
  LSX_BUILTIN (vseq_b, LARCH_V16QI_FTYPE_V16QI_V16QI),
  LSX_BUILTIN (vseq_h, LARCH_V8HI_FTYPE_V8HI_V8HI),
  LSX_BUILTIN (vseq_w, LARCH_V4SI_FTYPE_V4SI_V4SI),
  LSX_BUILTIN (vseq_d, LARCH_V2DI_FTYPE_V2DI_V2DI),
  LSX_BUILTIN (vseqi_b, LARCH_V16QI_FTYPE_V16QI_QI),
  LSX_BUILTIN (vseqi_h, LARCH_V8HI_FTYPE_V8HI_QI),
  LSX_BUILTIN (vseqi_w, LARCH_V4SI_FTYPE_V4SI_QI),
  LSX_BUILTIN (vseqi_d, LARCH_V2DI_FTYPE_V2DI_QI),
  LSX_BUILTIN (vslti_b, LARCH_V16QI_FTYPE_V16QI_QI),
  LSX_BUILTIN (vslt_b, LARCH_V16QI_FTYPE_V16QI_V16QI),
  LSX_BUILTIN (vslt_h, LARCH_V8HI_FTYPE_V8HI_V8HI),
  LSX_BUILTIN (vslt_w, LARCH_V4SI_FTYPE_V4SI_V4SI),
  LSX_BUILTIN (vslt_d, LARCH_V2DI_FTYPE_V2DI_V2DI),
  LSX_BUILTIN (vslti_h, LARCH_V8HI_FTYPE_V8HI_QI),
  LSX_BUILTIN (vslti_w, LARCH_V4SI_FTYPE_V4SI_QI),
  LSX_BUILTIN (vslti_d, LARCH_V2DI_FTYPE_V2DI_QI),
  LSX_BUILTIN (vslt_bu, LARCH_V16QI_FTYPE_UV16QI_UV16QI),
  LSX_BUILTIN (vslt_hu, LARCH_V8HI_FTYPE_UV8HI_UV8HI),
  LSX_BUILTIN (vslt_wu, LARCH_V4SI_FTYPE_UV4SI_UV4SI),
  LSX_BUILTIN (vslt_du, LARCH_V2DI_FTYPE_UV2DI_UV2DI),
  LSX_BUILTIN (vslti_bu, LARCH_V16QI_FTYPE_UV16QI_UQI),
  LSX_BUILTIN (vslti_hu, LARCH_V8HI_FTYPE_UV8HI_UQI),
  LSX_BUILTIN (vslti_wu, LARCH_V4SI_FTYPE_UV4SI_UQI),
  LSX_BUILTIN (vslti_du, LARCH_V2DI_FTYPE_UV2DI_UQI),
  LSX_BUILTIN (vsle_b, LARCH_V16QI_FTYPE_V16QI_V16QI),
  LSX_BUILTIN (vsle_h, LARCH_V8HI_FTYPE_V8HI_V8HI),
  LSX_BUILTIN (vsle_w, LARCH_V4SI_FTYPE_V4SI_V4SI),
  LSX_BUILTIN (vsle_d, LARCH_V2DI_FTYPE_V2DI_V2DI),
  LSX_BUILTIN (vslei_b, LARCH_V16QI_FTYPE_V16QI_QI),
  LSX_BUILTIN (vslei_h, LARCH_V8HI_FTYPE_V8HI_QI),
  LSX_BUILTIN (vslei_w, LARCH_V4SI_FTYPE_V4SI_QI),
  LSX_BUILTIN (vslei_d, LARCH_V2DI_FTYPE_V2DI_QI),
  LSX_BUILTIN (vsle_bu, LARCH_V16QI_FTYPE_UV16QI_UV16QI),
  LSX_BUILTIN (vsle_hu, LARCH_V8HI_FTYPE_UV8HI_UV8HI),
  LSX_BUILTIN (vsle_wu, LARCH_V4SI_FTYPE_UV4SI_UV4SI),
  LSX_BUILTIN (vsle_du, LARCH_V2DI_FTYPE_UV2DI_UV2DI),
  LSX_BUILTIN (vslei_bu, LARCH_V16QI_FTYPE_UV16QI_UQI),
  LSX_BUILTIN (vslei_hu, LARCH_V8HI_FTYPE_UV8HI_UQI),
  LSX_BUILTIN (vslei_wu, LARCH_V4SI_FTYPE_UV4SI_UQI),
  LSX_BUILTIN (vslei_du, LARCH_V2DI_FTYPE_UV2DI_UQI),
  LSX_BUILTIN (vsat_b, LARCH_V16QI_FTYPE_V16QI_UQI),
  LSX_BUILTIN (vsat_h, LARCH_V8HI_FTYPE_V8HI_UQI),
  LSX_BUILTIN (vsat_w, LARCH_V4SI_FTYPE_V4SI_UQI),
  LSX_BUILTIN (vsat_d, LARCH_V2DI_FTYPE_V2DI_UQI),
  LSX_BUILTIN (vsat_bu, LARCH_UV16QI_FTYPE_UV16QI_UQI),
  LSX_BUILTIN (vsat_hu, LARCH_UV8HI_FTYPE_UV8HI_UQI),
  LSX_BUILTIN (vsat_wu, LARCH_UV4SI_FTYPE_UV4SI_UQI),
  LSX_BUILTIN (vsat_du, LARCH_UV2DI_FTYPE_UV2DI_UQI),
  LSX_BUILTIN (vadda_b, LARCH_V16QI_FTYPE_V16QI_V16QI),
  LSX_BUILTIN (vadda_h, LARCH_V8HI_FTYPE_V8HI_V8HI),
  LSX_BUILTIN (vadda_w, LARCH_V4SI_FTYPE_V4SI_V4SI),
  LSX_BUILTIN (vadda_d, LARCH_V2DI_FTYPE_V2DI_V2DI),
  LSX_BUILTIN (vsadd_b, LARCH_V16QI_FTYPE_V16QI_V16QI),
  LSX_BUILTIN (vsadd_h, LARCH_V8HI_FTYPE_V8HI_V8HI),
  LSX_BUILTIN (vsadd_w, LARCH_V4SI_FTYPE_V4SI_V4SI),
  LSX_BUILTIN (vsadd_d, LARCH_V2DI_FTYPE_V2DI_V2DI),
  LSX_BUILTIN (vsadd_bu, LARCH_UV16QI_FTYPE_UV16QI_UV16QI),
  LSX_BUILTIN (vsadd_hu, LARCH_UV8HI_FTYPE_UV8HI_UV8HI),
  LSX_BUILTIN (vsadd_wu, LARCH_UV4SI_FTYPE_UV4SI_UV4SI),
  LSX_BUILTIN (vsadd_du, LARCH_UV2DI_FTYPE_UV2DI_UV2DI),
  LSX_BUILTIN (vavg_b, LARCH_V16QI_FTYPE_V16QI_V16QI),
  LSX_BUILTIN (vavg_h, LARCH_V8HI_FTYPE_V8HI_V8HI),
  LSX_BUILTIN (vavg_w, LARCH_V4SI_FTYPE_V4SI_V4SI),
  LSX_BUILTIN (vavg_d, LARCH_V2DI_FTYPE_V2DI_V2DI),
  LSX_BUILTIN (vavg_bu, LARCH_UV16QI_FTYPE_UV16QI_UV16QI),
  LSX_BUILTIN (vavg_hu, LARCH_UV8HI_FTYPE_UV8HI_UV8HI),
  LSX_BUILTIN (vavg_wu, LARCH_UV4SI_FTYPE_UV4SI_UV4SI),
  LSX_BUILTIN (vavg_du, LARCH_UV2DI_FTYPE_UV2DI_UV2DI),
  LSX_BUILTIN (vavgr_b, LARCH_V16QI_FTYPE_V16QI_V16QI),
  LSX_BUILTIN (vavgr_h, LARCH_V8HI_FTYPE_V8HI_V8HI),
  LSX_BUILTIN (vavgr_w, LARCH_V4SI_FTYPE_V4SI_V4SI),
  LSX_BUILTIN (vavgr_d, LARCH_V2DI_FTYPE_V2DI_V2DI),
  LSX_BUILTIN (vavgr_bu, LARCH_UV16QI_FTYPE_UV16QI_UV16QI),
  LSX_BUILTIN (vavgr_hu, LARCH_UV8HI_FTYPE_UV8HI_UV8HI),
  LSX_BUILTIN (vavgr_wu, LARCH_UV4SI_FTYPE_UV4SI_UV4SI),
  LSX_BUILTIN (vavgr_du, LARCH_UV2DI_FTYPE_UV2DI_UV2DI),
  LSX_BUILTIN (vssub_b, LARCH_V16QI_FTYPE_V16QI_V16QI),
  LSX_BUILTIN (vssub_h, LARCH_V8HI_FTYPE_V8HI_V8HI),
  LSX_BUILTIN (vssub_w, LARCH_V4SI_FTYPE_V4SI_V4SI),
  LSX_BUILTIN (vssub_d, LARCH_V2DI_FTYPE_V2DI_V2DI),
  LSX_BUILTIN (vssub_bu, LARCH_UV16QI_FTYPE_UV16QI_UV16QI),
  LSX_BUILTIN (vssub_hu, LARCH_UV8HI_FTYPE_UV8HI_UV8HI),
  LSX_BUILTIN (vssub_wu, LARCH_UV4SI_FTYPE_UV4SI_UV4SI),
  LSX_BUILTIN (vssub_du, LARCH_UV2DI_FTYPE_UV2DI_UV2DI),
  LSX_BUILTIN (vabsd_b, LARCH_V16QI_FTYPE_V16QI_V16QI),
  LSX_BUILTIN (vabsd_h, LARCH_V8HI_FTYPE_V8HI_V8HI),
  LSX_BUILTIN (vabsd_w, LARCH_V4SI_FTYPE_V4SI_V4SI),
  LSX_BUILTIN (vabsd_d, LARCH_V2DI_FTYPE_V2DI_V2DI),
  LSX_BUILTIN (vabsd_bu, LARCH_UV16QI_FTYPE_UV16QI_UV16QI),
  LSX_BUILTIN (vabsd_hu, LARCH_UV8HI_FTYPE_UV8HI_UV8HI),
  LSX_BUILTIN (vabsd_wu, LARCH_UV4SI_FTYPE_UV4SI_UV4SI),
  LSX_BUILTIN (vabsd_du, LARCH_UV2DI_FTYPE_UV2DI_UV2DI),
  LSX_BUILTIN (vmul_b, LARCH_V16QI_FTYPE_V16QI_V16QI),
  LSX_BUILTIN (vmul_h, LARCH_V8HI_FTYPE_V8HI_V8HI),
  LSX_BUILTIN (vmul_w, LARCH_V4SI_FTYPE_V4SI_V4SI),
  LSX_BUILTIN (vmul_d, LARCH_V2DI_FTYPE_V2DI_V2DI),
  LSX_BUILTIN (vmadd_b, LARCH_V16QI_FTYPE_V16QI_V16QI_V16QI),
  LSX_BUILTIN (vmadd_h, LARCH_V8HI_FTYPE_V8HI_V8HI_V8HI),
  LSX_BUILTIN (vmadd_w, LARCH_V4SI_FTYPE_V4SI_V4SI_V4SI),
  LSX_BUILTIN (vmadd_d, LARCH_V2DI_FTYPE_V2DI_V2DI_V2DI),
  LSX_BUILTIN (vmsub_b, LARCH_V16QI_FTYPE_V16QI_V16QI_V16QI),
  LSX_BUILTIN (vmsub_h, LARCH_V8HI_FTYPE_V8HI_V8HI_V8HI),
  LSX_BUILTIN (vmsub_w, LARCH_V4SI_FTYPE_V4SI_V4SI_V4SI),
  LSX_BUILTIN (vmsub_d, LARCH_V2DI_FTYPE_V2DI_V2DI_V2DI),
  LSX_BUILTIN (vdiv_b, LARCH_V16QI_FTYPE_V16QI_V16QI),
  LSX_BUILTIN (vdiv_h, LARCH_V8HI_FTYPE_V8HI_V8HI),
  LSX_BUILTIN (vdiv_w, LARCH_V4SI_FTYPE_V4SI_V4SI),
  LSX_BUILTIN (vdiv_d, LARCH_V2DI_FTYPE_V2DI_V2DI),
  LSX_BUILTIN (vdiv_bu, LARCH_UV16QI_FTYPE_UV16QI_UV16QI),
  LSX_BUILTIN (vdiv_hu, LARCH_UV8HI_FTYPE_UV8HI_UV8HI),
  LSX_BUILTIN (vdiv_wu, LARCH_UV4SI_FTYPE_UV4SI_UV4SI),
  LSX_BUILTIN (vdiv_du, LARCH_UV2DI_FTYPE_UV2DI_UV2DI),
  LSX_BUILTIN (vhaddw_h_b, LARCH_V8HI_FTYPE_V16QI_V16QI),
  LSX_BUILTIN (vhaddw_w_h, LARCH_V4SI_FTYPE_V8HI_V8HI),
  LSX_BUILTIN (vhaddw_d_w, LARCH_V2DI_FTYPE_V4SI_V4SI),
  LSX_BUILTIN (vhaddw_hu_bu, LARCH_UV8HI_FTYPE_UV16QI_UV16QI),
  LSX_BUILTIN (vhaddw_wu_hu, LARCH_UV4SI_FTYPE_UV8HI_UV8HI),
  LSX_BUILTIN (vhaddw_du_wu, LARCH_UV2DI_FTYPE_UV4SI_UV4SI),
  LSX_BUILTIN (vhsubw_h_b, LARCH_V8HI_FTYPE_V16QI_V16QI),
  LSX_BUILTIN (vhsubw_w_h, LARCH_V4SI_FTYPE_V8HI_V8HI),
  LSX_BUILTIN (vhsubw_d_w, LARCH_V2DI_FTYPE_V4SI_V4SI),
  LSX_BUILTIN (vhsubw_hu_bu, LARCH_V8HI_FTYPE_UV16QI_UV16QI),
  LSX_BUILTIN (vhsubw_wu_hu, LARCH_V4SI_FTYPE_UV8HI_UV8HI),
  LSX_BUILTIN (vhsubw_du_wu, LARCH_V2DI_FTYPE_UV4SI_UV4SI),
  LSX_BUILTIN (vmod_b, LARCH_V16QI_FTYPE_V16QI_V16QI),
  LSX_BUILTIN (vmod_h, LARCH_V8HI_FTYPE_V8HI_V8HI),
  LSX_BUILTIN (vmod_w, LARCH_V4SI_FTYPE_V4SI_V4SI),
  LSX_BUILTIN (vmod_d, LARCH_V2DI_FTYPE_V2DI_V2DI),
  LSX_BUILTIN (vmod_bu, LARCH_UV16QI_FTYPE_UV16QI_UV16QI),
  LSX_BUILTIN (vmod_hu, LARCH_UV8HI_FTYPE_UV8HI_UV8HI),
  LSX_BUILTIN (vmod_wu, LARCH_UV4SI_FTYPE_UV4SI_UV4SI),
  LSX_BUILTIN (vmod_du, LARCH_UV2DI_FTYPE_UV2DI_UV2DI),
  LSX_BUILTIN (vreplve_b, LARCH_V16QI_FTYPE_V16QI_SI),
  LSX_BUILTIN (vreplve_h, LARCH_V8HI_FTYPE_V8HI_SI),
  LSX_BUILTIN (vreplve_w, LARCH_V4SI_FTYPE_V4SI_SI),
  LSX_BUILTIN (vreplve_d, LARCH_V2DI_FTYPE_V2DI_SI),
  LSX_BUILTIN (vreplvei_b, LARCH_V16QI_FTYPE_V16QI_UQI),
  LSX_BUILTIN (vreplvei_h, LARCH_V8HI_FTYPE_V8HI_UQI),
  LSX_BUILTIN (vreplvei_w, LARCH_V4SI_FTYPE_V4SI_UQI),
  LSX_BUILTIN (vreplvei_d, LARCH_V2DI_FTYPE_V2DI_UQI),
  LSX_BUILTIN (vpickev_b, LARCH_V16QI_FTYPE_V16QI_V16QI),
  LSX_BUILTIN (vpickev_h, LARCH_V8HI_FTYPE_V8HI_V8HI),
  LSX_BUILTIN (vpickev_w, LARCH_V4SI_FTYPE_V4SI_V4SI),
  LSX_BUILTIN (vpickev_d, LARCH_V2DI_FTYPE_V2DI_V2DI),
  LSX_BUILTIN (vpickod_b, LARCH_V16QI_FTYPE_V16QI_V16QI),
  LSX_BUILTIN (vpickod_h, LARCH_V8HI_FTYPE_V8HI_V8HI),
  LSX_BUILTIN (vpickod_w, LARCH_V4SI_FTYPE_V4SI_V4SI),
  LSX_BUILTIN (vpickod_d, LARCH_V2DI_FTYPE_V2DI_V2DI),
  LSX_BUILTIN (vilvh_b, LARCH_V16QI_FTYPE_V16QI_V16QI),
  LSX_BUILTIN (vilvh_h, LARCH_V8HI_FTYPE_V8HI_V8HI),
  LSX_BUILTIN (vilvh_w, LARCH_V4SI_FTYPE_V4SI_V4SI),
  LSX_BUILTIN (vilvh_d, LARCH_V2DI_FTYPE_V2DI_V2DI),
  LSX_BUILTIN (vilvl_b, LARCH_V16QI_FTYPE_V16QI_V16QI),
  LSX_BUILTIN (vilvl_h, LARCH_V8HI_FTYPE_V8HI_V8HI),
  LSX_BUILTIN (vilvl_w, LARCH_V4SI_FTYPE_V4SI_V4SI),
  LSX_BUILTIN (vilvl_d, LARCH_V2DI_FTYPE_V2DI_V2DI),
  LSX_BUILTIN (vpackev_b, LARCH_V16QI_FTYPE_V16QI_V16QI),
  LSX_BUILTIN (vpackev_h, LARCH_V8HI_FTYPE_V8HI_V8HI),
  LSX_BUILTIN (vpackev_w, LARCH_V4SI_FTYPE_V4SI_V4SI),
  LSX_BUILTIN (vpackev_d, LARCH_V2DI_FTYPE_V2DI_V2DI),
  LSX_BUILTIN (vpackod_b, LARCH_V16QI_FTYPE_V16QI_V16QI),
  LSX_BUILTIN (vpackod_h, LARCH_V8HI_FTYPE_V8HI_V8HI),
  LSX_BUILTIN (vpackod_w, LARCH_V4SI_FTYPE_V4SI_V4SI),
  LSX_BUILTIN (vpackod_d, LARCH_V2DI_FTYPE_V2DI_V2DI),
  LSX_BUILTIN (vshuf_h, LARCH_V8HI_FTYPE_V8HI_V8HI_V8HI),
  LSX_BUILTIN (vshuf_w, LARCH_V4SI_FTYPE_V4SI_V4SI_V4SI),
  LSX_BUILTIN (vshuf_d, LARCH_V2DI_FTYPE_V2DI_V2DI_V2DI),
  LSX_BUILTIN (vand_v, LARCH_UV16QI_FTYPE_UV16QI_UV16QI),
  LSX_BUILTIN (vandi_b, LARCH_UV16QI_FTYPE_UV16QI_UQI),
  LSX_BUILTIN (vor_v, LARCH_UV16QI_FTYPE_UV16QI_UV16QI),
  LSX_BUILTIN (vori_b, LARCH_UV16QI_FTYPE_UV16QI_UQI),
  LSX_BUILTIN (vnor_v, LARCH_UV16QI_FTYPE_UV16QI_UV16QI),
  LSX_BUILTIN (vnori_b, LARCH_UV16QI_FTYPE_UV16QI_UQI),
  LSX_BUILTIN (vxor_v, LARCH_UV16QI_FTYPE_UV16QI_UV16QI),
  LSX_BUILTIN (vxori_b, LARCH_UV16QI_FTYPE_UV16QI_UQI),
  LSX_BUILTIN (vbitsel_v, LARCH_UV16QI_FTYPE_UV16QI_UV16QI_UV16QI),
  LSX_BUILTIN (vbitseli_b, LARCH_UV16QI_FTYPE_UV16QI_UV16QI_USI),
  LSX_BUILTIN (vshuf4i_b, LARCH_V16QI_FTYPE_V16QI_USI),
  LSX_BUILTIN (vshuf4i_h, LARCH_V8HI_FTYPE_V8HI_USI),
  LSX_BUILTIN (vshuf4i_w, LARCH_V4SI_FTYPE_V4SI_USI),
  LSX_BUILTIN (vreplgr2vr_b, LARCH_V16QI_FTYPE_SI),
  LSX_BUILTIN (vreplgr2vr_h, LARCH_V8HI_FTYPE_SI),
  LSX_BUILTIN (vreplgr2vr_w, LARCH_V4SI_FTYPE_SI),
  LSX_BUILTIN (vreplgr2vr_d, LARCH_V2DI_FTYPE_DI),
  LSX_BUILTIN (vpcnt_b, LARCH_V16QI_FTYPE_V16QI),
  LSX_BUILTIN (vpcnt_h, LARCH_V8HI_FTYPE_V8HI),
  LSX_BUILTIN (vpcnt_w, LARCH_V4SI_FTYPE_V4SI),
  LSX_BUILTIN (vpcnt_d, LARCH_V2DI_FTYPE_V2DI),
  LSX_BUILTIN (vclo_b, LARCH_V16QI_FTYPE_V16QI),
  LSX_BUILTIN (vclo_h, LARCH_V8HI_FTYPE_V8HI),
  LSX_BUILTIN (vclo_w, LARCH_V4SI_FTYPE_V4SI),
  LSX_BUILTIN (vclo_d, LARCH_V2DI_FTYPE_V2DI),
  LSX_BUILTIN (vclz_b, LARCH_V16QI_FTYPE_V16QI),
  LSX_BUILTIN (vclz_h, LARCH_V8HI_FTYPE_V8HI),
  LSX_BUILTIN (vclz_w, LARCH_V4SI_FTYPE_V4SI),
  LSX_BUILTIN (vclz_d, LARCH_V2DI_FTYPE_V2DI),
  LSX_BUILTIN (vpickve2gr_b, LARCH_SI_FTYPE_V16QI_UQI),
  LSX_BUILTIN (vpickve2gr_h, LARCH_SI_FTYPE_V8HI_UQI),
  LSX_BUILTIN (vpickve2gr_w, LARCH_SI_FTYPE_V4SI_UQI),
  LSX_BUILTIN (vpickve2gr_d, LARCH_DI_FTYPE_V2DI_UQI),
  LSX_BUILTIN (vpickve2gr_bu, LARCH_USI_FTYPE_V16QI_UQI),
  LSX_BUILTIN (vpickve2gr_hu, LARCH_USI_FTYPE_V8HI_UQI),
  LSX_BUILTIN (vpickve2gr_wu, LARCH_USI_FTYPE_V4SI_UQI),
  LSX_BUILTIN (vpickve2gr_du, LARCH_UDI_FTYPE_V2DI_UQI),
  LSX_BUILTIN (vinsgr2vr_b, LARCH_V16QI_FTYPE_V16QI_SI_UQI),
  LSX_BUILTIN (vinsgr2vr_h, LARCH_V8HI_FTYPE_V8HI_SI_UQI),
  LSX_BUILTIN (vinsgr2vr_w, LARCH_V4SI_FTYPE_V4SI_SI_UQI),
  LSX_BUILTIN (vinsgr2vr_d, LARCH_V2DI_FTYPE_V2DI_DI_UQI),
  LSX_BUILTIN_TEST_BRANCH (bnz_b, LARCH_SI_FTYPE_UV16QI),
  LSX_BUILTIN_TEST_BRANCH (bnz_h, LARCH_SI_FTYPE_UV8HI),
  LSX_BUILTIN_TEST_BRANCH (bnz_w, LARCH_SI_FTYPE_UV4SI),
  LSX_BUILTIN_TEST_BRANCH (bnz_d, LARCH_SI_FTYPE_UV2DI),
  LSX_BUILTIN_TEST_BRANCH (bz_b, LARCH_SI_FTYPE_UV16QI),
  LSX_BUILTIN_TEST_BRANCH (bz_h, LARCH_SI_FTYPE_UV8HI),
  LSX_BUILTIN_TEST_BRANCH (bz_w, LARCH_SI_FTYPE_UV4SI),
  LSX_BUILTIN_TEST_BRANCH (bz_d, LARCH_SI_FTYPE_UV2DI),
  LSX_BUILTIN_TEST_BRANCH (bz_v, LARCH_SI_FTYPE_UV16QI),
  LSX_BUILTIN_TEST_BRANCH (bnz_v,	LARCH_SI_FTYPE_UV16QI),
  LSX_BUILTIN (vrepli_b, LARCH_V16QI_FTYPE_HI),
  LSX_BUILTIN (vrepli_h, LARCH_V8HI_FTYPE_HI),
  LSX_BUILTIN (vrepli_w, LARCH_V4SI_FTYPE_HI),
  LSX_BUILTIN (vrepli_d, LARCH_V2DI_FTYPE_HI),
  LSX_BUILTIN (vfcmp_caf_s, LARCH_V4SI_FTYPE_V4SF_V4SF),
  LSX_BUILTIN (vfcmp_caf_d, LARCH_V2DI_FTYPE_V2DF_V2DF),
  LSX_BUILTIN (vfcmp_cor_s, LARCH_V4SI_FTYPE_V4SF_V4SF),
  LSX_BUILTIN (vfcmp_cor_d, LARCH_V2DI_FTYPE_V2DF_V2DF),
  LSX_BUILTIN (vfcmp_cun_s, LARCH_V4SI_FTYPE_V4SF_V4SF),
  LSX_BUILTIN (vfcmp_cun_d, LARCH_V2DI_FTYPE_V2DF_V2DF),
  LSX_BUILTIN (vfcmp_cune_s, LARCH_V4SI_FTYPE_V4SF_V4SF),
  LSX_BUILTIN (vfcmp_cune_d, LARCH_V2DI_FTYPE_V2DF_V2DF),
  LSX_BUILTIN (vfcmp_cueq_s, LARCH_V4SI_FTYPE_V4SF_V4SF),
  LSX_BUILTIN (vfcmp_cueq_d, LARCH_V2DI_FTYPE_V2DF_V2DF),
  LSX_BUILTIN (vfcmp_ceq_s, LARCH_V4SI_FTYPE_V4SF_V4SF),
  LSX_BUILTIN (vfcmp_ceq_d, LARCH_V2DI_FTYPE_V2DF_V2DF),
  LSX_BUILTIN (vfcmp_cne_s, LARCH_V4SI_FTYPE_V4SF_V4SF),
  LSX_BUILTIN (vfcmp_cne_d, LARCH_V2DI_FTYPE_V2DF_V2DF),
  LSX_BUILTIN (vfcmp_clt_s, LARCH_V4SI_FTYPE_V4SF_V4SF),
  LSX_BUILTIN (vfcmp_clt_d, LARCH_V2DI_FTYPE_V2DF_V2DF),
  LSX_BUILTIN (vfcmp_cult_s, LARCH_V4SI_FTYPE_V4SF_V4SF),
  LSX_BUILTIN (vfcmp_cult_d, LARCH_V2DI_FTYPE_V2DF_V2DF),
  LSX_BUILTIN (vfcmp_cle_s, LARCH_V4SI_FTYPE_V4SF_V4SF),
  LSX_BUILTIN (vfcmp_cle_d, LARCH_V2DI_FTYPE_V2DF_V2DF),
  LSX_BUILTIN (vfcmp_cule_s, LARCH_V4SI_FTYPE_V4SF_V4SF),
  LSX_BUILTIN (vfcmp_cule_d, LARCH_V2DI_FTYPE_V2DF_V2DF),
  LSX_BUILTIN (vfcmp_saf_s, LARCH_V4SI_FTYPE_V4SF_V4SF),
  LSX_BUILTIN (vfcmp_saf_d, LARCH_V2DI_FTYPE_V2DF_V2DF),
  LSX_BUILTIN (vfcmp_sor_s, LARCH_V4SI_FTYPE_V4SF_V4SF),
  LSX_BUILTIN (vfcmp_sor_d, LARCH_V2DI_FTYPE_V2DF_V2DF),
  LSX_BUILTIN (vfcmp_sun_s, LARCH_V4SI_FTYPE_V4SF_V4SF),
  LSX_BUILTIN (vfcmp_sun_d, LARCH_V2DI_FTYPE_V2DF_V2DF),
  LSX_BUILTIN (vfcmp_sune_s, LARCH_V4SI_FTYPE_V4SF_V4SF),
  LSX_BUILTIN (vfcmp_sune_d, LARCH_V2DI_FTYPE_V2DF_V2DF),
  LSX_BUILTIN (vfcmp_sueq_s, LARCH_V4SI_FTYPE_V4SF_V4SF),
  LSX_BUILTIN (vfcmp_sueq_d, LARCH_V2DI_FTYPE_V2DF_V2DF),
  LSX_BUILTIN (vfcmp_seq_s, LARCH_V4SI_FTYPE_V4SF_V4SF),
  LSX_BUILTIN (vfcmp_seq_d, LARCH_V2DI_FTYPE_V2DF_V2DF),
  LSX_BUILTIN (vfcmp_sne_s, LARCH_V4SI_FTYPE_V4SF_V4SF),
  LSX_BUILTIN (vfcmp_sne_d, LARCH_V2DI_FTYPE_V2DF_V2DF),
  LSX_BUILTIN (vfcmp_slt_s, LARCH_V4SI_FTYPE_V4SF_V4SF),
  LSX_BUILTIN (vfcmp_slt_d, LARCH_V2DI_FTYPE_V2DF_V2DF),
  LSX_BUILTIN (vfcmp_sult_s, LARCH_V4SI_FTYPE_V4SF_V4SF),
  LSX_BUILTIN (vfcmp_sult_d, LARCH_V2DI_FTYPE_V2DF_V2DF),
  LSX_BUILTIN (vfcmp_sle_s, LARCH_V4SI_FTYPE_V4SF_V4SF),
  LSX_BUILTIN (vfcmp_sle_d, LARCH_V2DI_FTYPE_V2DF_V2DF),
  LSX_BUILTIN (vfcmp_sule_s, LARCH_V4SI_FTYPE_V4SF_V4SF),
  LSX_BUILTIN (vfcmp_sule_d, LARCH_V2DI_FTYPE_V2DF_V2DF),
  LSX_BUILTIN (vfadd_s, LARCH_V4SF_FTYPE_V4SF_V4SF),
  LSX_BUILTIN (vfadd_d, LARCH_V2DF_FTYPE_V2DF_V2DF),
  LSX_BUILTIN (vfsub_s, LARCH_V4SF_FTYPE_V4SF_V4SF),
  LSX_BUILTIN (vfsub_d, LARCH_V2DF_FTYPE_V2DF_V2DF),
  LSX_BUILTIN (vfmul_s, LARCH_V4SF_FTYPE_V4SF_V4SF),
  LSX_BUILTIN (vfmul_d, LARCH_V2DF_FTYPE_V2DF_V2DF),
  LSX_BUILTIN (vfdiv_s, LARCH_V4SF_FTYPE_V4SF_V4SF),
  LSX_BUILTIN (vfdiv_d, LARCH_V2DF_FTYPE_V2DF_V2DF),
  LSX_BUILTIN (vfcvt_h_s, LARCH_V8HI_FTYPE_V4SF_V4SF),
  LSX_BUILTIN (vfcvt_s_d, LARCH_V4SF_FTYPE_V2DF_V2DF),
  LSX_BUILTIN (vfmin_s, LARCH_V4SF_FTYPE_V4SF_V4SF),
  LSX_BUILTIN (vfmin_d, LARCH_V2DF_FTYPE_V2DF_V2DF),
  LSX_BUILTIN (vfmina_s, LARCH_V4SF_FTYPE_V4SF_V4SF),
  LSX_BUILTIN (vfmina_d, LARCH_V2DF_FTYPE_V2DF_V2DF),
  LSX_BUILTIN (vfmax_s, LARCH_V4SF_FTYPE_V4SF_V4SF),
  LSX_BUILTIN (vfmax_d, LARCH_V2DF_FTYPE_V2DF_V2DF),
  LSX_BUILTIN (vfmaxa_s, LARCH_V4SF_FTYPE_V4SF_V4SF),
  LSX_BUILTIN (vfmaxa_d, LARCH_V2DF_FTYPE_V2DF_V2DF),
  LSX_BUILTIN (vfclass_s, LARCH_V4SI_FTYPE_V4SF),
  LSX_BUILTIN (vfclass_d, LARCH_V2DI_FTYPE_V2DF),
  LSX_BUILTIN (vfsqrt_s, LARCH_V4SF_FTYPE_V4SF),
  LSX_BUILTIN (vfsqrt_d, LARCH_V2DF_FTYPE_V2DF),
  LSX_BUILTIN (vfrecip_s, LARCH_V4SF_FTYPE_V4SF),
  LSX_BUILTIN (vfrecip_d, LARCH_V2DF_FTYPE_V2DF),
  LSX_BUILTIN (vfrint_s, LARCH_V4SF_FTYPE_V4SF),
  LSX_BUILTIN (vfrint_d, LARCH_V2DF_FTYPE_V2DF),
  LSX_BUILTIN (vfrsqrt_s, LARCH_V4SF_FTYPE_V4SF),
  LSX_BUILTIN (vfrsqrt_d, LARCH_V2DF_FTYPE_V2DF),
  LSX_BUILTIN (vflogb_s, LARCH_V4SF_FTYPE_V4SF),
  LSX_BUILTIN (vflogb_d, LARCH_V2DF_FTYPE_V2DF),
  LSX_BUILTIN (vfcvth_s_h, LARCH_V4SF_FTYPE_V8HI),
  LSX_BUILTIN (vfcvth_d_s, LARCH_V2DF_FTYPE_V4SF),
  LSX_BUILTIN (vfcvtl_s_h, LARCH_V4SF_FTYPE_V8HI),
  LSX_BUILTIN (vfcvtl_d_s, LARCH_V2DF_FTYPE_V4SF),
  LSX_BUILTIN (vftint_w_s, LARCH_V4SI_FTYPE_V4SF),
  LSX_BUILTIN (vftint_l_d, LARCH_V2DI_FTYPE_V2DF),
  LSX_BUILTIN (vftint_wu_s, LARCH_UV4SI_FTYPE_V4SF),
  LSX_BUILTIN (vftint_lu_d, LARCH_UV2DI_FTYPE_V2DF),
  LSX_BUILTIN (vftintrz_w_s, LARCH_V4SI_FTYPE_V4SF),
  LSX_BUILTIN (vftintrz_l_d, LARCH_V2DI_FTYPE_V2DF),
  LSX_BUILTIN (vftintrz_wu_s, LARCH_UV4SI_FTYPE_V4SF),
  LSX_BUILTIN (vftintrz_lu_d, LARCH_UV2DI_FTYPE_V2DF),
  LSX_BUILTIN (vffint_s_w, LARCH_V4SF_FTYPE_V4SI),
  LSX_BUILTIN (vffint_d_l, LARCH_V2DF_FTYPE_V2DI),
  LSX_BUILTIN (vffint_s_wu, LARCH_V4SF_FTYPE_UV4SI),
  LSX_BUILTIN (vffint_d_lu, LARCH_V2DF_FTYPE_UV2DI),

  LSX_BUILTIN (vandn_v, LARCH_UV16QI_FTYPE_UV16QI_UV16QI),
  LSX_BUILTIN (vneg_b, LARCH_V16QI_FTYPE_V16QI),
  LSX_BUILTIN (vneg_h, LARCH_V8HI_FTYPE_V8HI),
  LSX_BUILTIN (vneg_w, LARCH_V4SI_FTYPE_V4SI),
  LSX_BUILTIN (vneg_d, LARCH_V2DI_FTYPE_V2DI),
  LSX_BUILTIN (vmuh_b, LARCH_V16QI_FTYPE_V16QI_V16QI),
  LSX_BUILTIN (vmuh_h, LARCH_V8HI_FTYPE_V8HI_V8HI),
  LSX_BUILTIN (vmuh_w, LARCH_V4SI_FTYPE_V4SI_V4SI),
  LSX_BUILTIN (vmuh_d, LARCH_V2DI_FTYPE_V2DI_V2DI),
  LSX_BUILTIN (vmuh_bu, LARCH_UV16QI_FTYPE_UV16QI_UV16QI),
  LSX_BUILTIN (vmuh_hu, LARCH_UV8HI_FTYPE_UV8HI_UV8HI),
  LSX_BUILTIN (vmuh_wu, LARCH_UV4SI_FTYPE_UV4SI_UV4SI),
  LSX_BUILTIN (vmuh_du, LARCH_UV2DI_FTYPE_UV2DI_UV2DI),
  LSX_BUILTIN (vsllwil_h_b, LARCH_V8HI_FTYPE_V16QI_UQI),
  LSX_BUILTIN (vsllwil_w_h, LARCH_V4SI_FTYPE_V8HI_UQI),
  LSX_BUILTIN (vsllwil_d_w, LARCH_V2DI_FTYPE_V4SI_UQI),
  LSX_BUILTIN (vsllwil_hu_bu, LARCH_UV8HI_FTYPE_UV16QI_UQI),
  LSX_BUILTIN (vsllwil_wu_hu, LARCH_UV4SI_FTYPE_UV8HI_UQI),
  LSX_BUILTIN (vsllwil_du_wu, LARCH_UV2DI_FTYPE_UV4SI_UQI),
  LSX_BUILTIN (vsran_b_h, LARCH_V16QI_FTYPE_V8HI_V8HI),
  LSX_BUILTIN (vsran_h_w, LARCH_V8HI_FTYPE_V4SI_V4SI),
  LSX_BUILTIN (vsran_w_d, LARCH_V4SI_FTYPE_V2DI_V2DI),
  LSX_BUILTIN (vssran_b_h, LARCH_V16QI_FTYPE_V8HI_V8HI),
  LSX_BUILTIN (vssran_h_w, LARCH_V8HI_FTYPE_V4SI_V4SI),
  LSX_BUILTIN (vssran_w_d, LARCH_V4SI_FTYPE_V2DI_V2DI),
  LSX_BUILTIN (vssran_bu_h, LARCH_UV16QI_FTYPE_UV8HI_UV8HI),
  LSX_BUILTIN (vssran_hu_w, LARCH_UV8HI_FTYPE_UV4SI_UV4SI),
  LSX_BUILTIN (vssran_wu_d, LARCH_UV4SI_FTYPE_UV2DI_UV2DI),
  LSX_BUILTIN (vsrarn_b_h, LARCH_V16QI_FTYPE_V8HI_V8HI),
  LSX_BUILTIN (vsrarn_h_w, LARCH_V8HI_FTYPE_V4SI_V4SI),
  LSX_BUILTIN (vsrarn_w_d, LARCH_V4SI_FTYPE_V2DI_V2DI),
  LSX_BUILTIN (vssrarn_b_h, LARCH_V16QI_FTYPE_V8HI_V8HI),
  LSX_BUILTIN (vssrarn_h_w, LARCH_V8HI_FTYPE_V4SI_V4SI),
  LSX_BUILTIN (vssrarn_w_d, LARCH_V4SI_FTYPE_V2DI_V2DI),
  LSX_BUILTIN (vssrarn_bu_h, LARCH_UV16QI_FTYPE_UV8HI_UV8HI),
  LSX_BUILTIN (vssrarn_hu_w, LARCH_UV8HI_FTYPE_UV4SI_UV4SI),
  LSX_BUILTIN (vssrarn_wu_d, LARCH_UV4SI_FTYPE_UV2DI_UV2DI),
  LSX_BUILTIN (vsrln_b_h, LARCH_V16QI_FTYPE_V8HI_V8HI),
  LSX_BUILTIN (vsrln_h_w, LARCH_V8HI_FTYPE_V4SI_V4SI),
  LSX_BUILTIN (vsrln_w_d, LARCH_V4SI_FTYPE_V2DI_V2DI),
  LSX_BUILTIN (vssrln_bu_h, LARCH_UV16QI_FTYPE_UV8HI_UV8HI),
  LSX_BUILTIN (vssrln_hu_w, LARCH_UV8HI_FTYPE_UV4SI_UV4SI),
  LSX_BUILTIN (vssrln_wu_d, LARCH_UV4SI_FTYPE_UV2DI_UV2DI),
  LSX_BUILTIN (vsrlrn_b_h, LARCH_V16QI_FTYPE_V8HI_V8HI),
  LSX_BUILTIN (vsrlrn_h_w, LARCH_V8HI_FTYPE_V4SI_V4SI),
  LSX_BUILTIN (vsrlrn_w_d, LARCH_V4SI_FTYPE_V2DI_V2DI),
  LSX_BUILTIN (vssrlrn_bu_h, LARCH_UV16QI_FTYPE_UV8HI_UV8HI),
  LSX_BUILTIN (vssrlrn_hu_w, LARCH_UV8HI_FTYPE_UV4SI_UV4SI),
  LSX_BUILTIN (vssrlrn_wu_d, LARCH_UV4SI_FTYPE_UV2DI_UV2DI),
  LSX_BUILTIN (vfrstpi_b, LARCH_V16QI_FTYPE_V16QI_V16QI_UQI),
  LSX_BUILTIN (vfrstpi_h, LARCH_V8HI_FTYPE_V8HI_V8HI_UQI),
  LSX_BUILTIN (vfrstp_b, LARCH_V16QI_FTYPE_V16QI_V16QI_V16QI),
  LSX_BUILTIN (vfrstp_h, LARCH_V8HI_FTYPE_V8HI_V8HI_V8HI),
  LSX_BUILTIN (vshuf4i_d, LARCH_V2DI_FTYPE_V2DI_V2DI_USI),
  LSX_BUILTIN (vbsrl_v, LARCH_V16QI_FTYPE_V16QI_UQI),
  LSX_BUILTIN (vbsll_v, LARCH_V16QI_FTYPE_V16QI_UQI),
  LSX_BUILTIN (vextrins_b, LARCH_V16QI_FTYPE_V16QI_V16QI_USI),
  LSX_BUILTIN (vextrins_h, LARCH_V8HI_FTYPE_V8HI_V8HI_USI),
  LSX_BUILTIN (vextrins_w, LARCH_V4SI_FTYPE_V4SI_V4SI_USI),
  LSX_BUILTIN (vextrins_d, LARCH_V2DI_FTYPE_V2DI_V2DI_USI),
  LSX_BUILTIN (vmskltz_b, LARCH_V16QI_FTYPE_V16QI),
  LSX_BUILTIN (vmskltz_h, LARCH_V8HI_FTYPE_V8HI),
  LSX_BUILTIN (vmskltz_w, LARCH_V4SI_FTYPE_V4SI),
  LSX_BUILTIN (vmskltz_d, LARCH_V2DI_FTYPE_V2DI),
  LSX_BUILTIN (vsigncov_b, LARCH_V16QI_FTYPE_V16QI_V16QI),
  LSX_BUILTIN (vsigncov_h, LARCH_V8HI_FTYPE_V8HI_V8HI),
  LSX_BUILTIN (vsigncov_w, LARCH_V4SI_FTYPE_V4SI_V4SI),
  LSX_BUILTIN (vsigncov_d, LARCH_V2DI_FTYPE_V2DI_V2DI),
  LSX_BUILTIN (vfmadd_s, LARCH_V4SF_FTYPE_V4SF_V4SF_V4SF),
  LSX_BUILTIN (vfmadd_d, LARCH_V2DF_FTYPE_V2DF_V2DF_V2DF),
  LSX_BUILTIN (vfmsub_s, LARCH_V4SF_FTYPE_V4SF_V4SF_V4SF),
  LSX_BUILTIN (vfmsub_d, LARCH_V2DF_FTYPE_V2DF_V2DF_V2DF),
  LSX_BUILTIN (vfnmadd_s, LARCH_V4SF_FTYPE_V4SF_V4SF_V4SF),
  LSX_BUILTIN (vfnmadd_d, LARCH_V2DF_FTYPE_V2DF_V2DF_V2DF),
  LSX_BUILTIN (vfnmsub_s, LARCH_V4SF_FTYPE_V4SF_V4SF_V4SF),
  LSX_BUILTIN (vfnmsub_d, LARCH_V2DF_FTYPE_V2DF_V2DF_V2DF),
  LSX_BUILTIN (vftintrne_w_s, LARCH_V4SI_FTYPE_V4SF),
  LSX_BUILTIN (vftintrne_l_d, LARCH_V2DI_FTYPE_V2DF),
  LSX_BUILTIN (vftintrp_w_s, LARCH_V4SI_FTYPE_V4SF),
  LSX_BUILTIN (vftintrp_l_d, LARCH_V2DI_FTYPE_V2DF),
  LSX_BUILTIN (vftintrm_w_s, LARCH_V4SI_FTYPE_V4SF),
  LSX_BUILTIN (vftintrm_l_d, LARCH_V2DI_FTYPE_V2DF),
  LSX_BUILTIN (vftint_w_d, LARCH_V4SI_FTYPE_V2DF_V2DF),
  LSX_BUILTIN (vffint_s_l, LARCH_V4SF_FTYPE_V2DI_V2DI),
  LSX_BUILTIN (vftintrz_w_d, LARCH_V4SI_FTYPE_V2DF_V2DF),
  LSX_BUILTIN (vftintrp_w_d, LARCH_V4SI_FTYPE_V2DF_V2DF),
  LSX_BUILTIN (vftintrm_w_d, LARCH_V4SI_FTYPE_V2DF_V2DF),
  LSX_BUILTIN (vftintrne_w_d, LARCH_V4SI_FTYPE_V2DF_V2DF),
  LSX_BUILTIN (vftintl_l_s, LARCH_V2DI_FTYPE_V4SF),
  LSX_BUILTIN (vftinth_l_s, LARCH_V2DI_FTYPE_V4SF),
  LSX_BUILTIN (vffinth_d_w, LARCH_V2DF_FTYPE_V4SI),
  LSX_BUILTIN (vffintl_d_w, LARCH_V2DF_FTYPE_V4SI),
  LSX_BUILTIN (vftintrzl_l_s, LARCH_V2DI_FTYPE_V4SF),
  LSX_BUILTIN (vftintrzh_l_s, LARCH_V2DI_FTYPE_V4SF),
  LSX_BUILTIN (vftintrpl_l_s, LARCH_V2DI_FTYPE_V4SF),
  LSX_BUILTIN (vftintrph_l_s, LARCH_V2DI_FTYPE_V4SF),
  LSX_BUILTIN (vftintrml_l_s, LARCH_V2DI_FTYPE_V4SF),
  LSX_BUILTIN (vftintrmh_l_s, LARCH_V2DI_FTYPE_V4SF),
  LSX_BUILTIN (vftintrnel_l_s, LARCH_V2DI_FTYPE_V4SF),
  LSX_BUILTIN (vftintrneh_l_s, LARCH_V2DI_FTYPE_V4SF),
  LSX_BUILTIN (vfrintrne_s, LARCH_V4SF_FTYPE_V4SF),
  LSX_BUILTIN (vfrintrne_d, LARCH_V2DF_FTYPE_V2DF),
  LSX_BUILTIN (vfrintrz_s, LARCH_V4SF_FTYPE_V4SF),
  LSX_BUILTIN (vfrintrz_d, LARCH_V2DF_FTYPE_V2DF),
  LSX_BUILTIN (vfrintrp_s, LARCH_V4SF_FTYPE_V4SF),
  LSX_BUILTIN (vfrintrp_d, LARCH_V2DF_FTYPE_V2DF),
  LSX_BUILTIN (vfrintrm_s, LARCH_V4SF_FTYPE_V4SF),
  LSX_BUILTIN (vfrintrm_d, LARCH_V2DF_FTYPE_V2DF),
  LSX_NO_TARGET_BUILTIN (vstelm_b, LARCH_VOID_FTYPE_V16QI_CVPOINTER_SI_UQI),
  LSX_NO_TARGET_BUILTIN (vstelm_h, LARCH_VOID_FTYPE_V8HI_CVPOINTER_SI_UQI),
  LSX_NO_TARGET_BUILTIN (vstelm_w, LARCH_VOID_FTYPE_V4SI_CVPOINTER_SI_UQI),
  LSX_NO_TARGET_BUILTIN (vstelm_d, LARCH_VOID_FTYPE_V2DI_CVPOINTER_SI_UQI),
  LSX_BUILTIN (vaddwev_d_w, LARCH_V2DI_FTYPE_V4SI_V4SI),
  LSX_BUILTIN (vaddwev_w_h, LARCH_V4SI_FTYPE_V8HI_V8HI),
  LSX_BUILTIN (vaddwev_h_b, LARCH_V8HI_FTYPE_V16QI_V16QI),
  LSX_BUILTIN (vaddwod_d_w, LARCH_V2DI_FTYPE_V4SI_V4SI),
  LSX_BUILTIN (vaddwod_w_h, LARCH_V4SI_FTYPE_V8HI_V8HI),
  LSX_BUILTIN (vaddwod_h_b, LARCH_V8HI_FTYPE_V16QI_V16QI),
  LSX_BUILTIN (vaddwev_d_wu, LARCH_V2DI_FTYPE_UV4SI_UV4SI),
  LSX_BUILTIN (vaddwev_w_hu, LARCH_V4SI_FTYPE_UV8HI_UV8HI),
  LSX_BUILTIN (vaddwev_h_bu, LARCH_V8HI_FTYPE_UV16QI_UV16QI),
  LSX_BUILTIN (vaddwod_d_wu, LARCH_V2DI_FTYPE_UV4SI_UV4SI),
  LSX_BUILTIN (vaddwod_w_hu, LARCH_V4SI_FTYPE_UV8HI_UV8HI),
  LSX_BUILTIN (vaddwod_h_bu, LARCH_V8HI_FTYPE_UV16QI_UV16QI),
  LSX_BUILTIN (vaddwev_d_wu_w, LARCH_V2DI_FTYPE_UV4SI_V4SI),
  LSX_BUILTIN (vaddwev_w_hu_h, LARCH_V4SI_FTYPE_UV8HI_V8HI),
  LSX_BUILTIN (vaddwev_h_bu_b, LARCH_V8HI_FTYPE_UV16QI_V16QI),
  LSX_BUILTIN (vaddwod_d_wu_w, LARCH_V2DI_FTYPE_UV4SI_V4SI),
  LSX_BUILTIN (vaddwod_w_hu_h, LARCH_V4SI_FTYPE_UV8HI_V8HI),
  LSX_BUILTIN (vaddwod_h_bu_b, LARCH_V8HI_FTYPE_UV16QI_V16QI),
  LSX_BUILTIN (vsubwev_d_w, LARCH_V2DI_FTYPE_V4SI_V4SI),
  LSX_BUILTIN (vsubwev_w_h, LARCH_V4SI_FTYPE_V8HI_V8HI),
  LSX_BUILTIN (vsubwev_h_b, LARCH_V8HI_FTYPE_V16QI_V16QI),
  LSX_BUILTIN (vsubwod_d_w, LARCH_V2DI_FTYPE_V4SI_V4SI),
  LSX_BUILTIN (vsubwod_w_h, LARCH_V4SI_FTYPE_V8HI_V8HI),
  LSX_BUILTIN (vsubwod_h_b, LARCH_V8HI_FTYPE_V16QI_V16QI),
  LSX_BUILTIN (vsubwev_d_wu, LARCH_V2DI_FTYPE_UV4SI_UV4SI),
  LSX_BUILTIN (vsubwev_w_hu, LARCH_V4SI_FTYPE_UV8HI_UV8HI),
  LSX_BUILTIN (vsubwev_h_bu, LARCH_V8HI_FTYPE_UV16QI_UV16QI),
  LSX_BUILTIN (vsubwod_d_wu, LARCH_V2DI_FTYPE_UV4SI_UV4SI),
  LSX_BUILTIN (vsubwod_w_hu, LARCH_V4SI_FTYPE_UV8HI_UV8HI),
  LSX_BUILTIN (vsubwod_h_bu, LARCH_V8HI_FTYPE_UV16QI_UV16QI),
  LSX_BUILTIN (vaddwev_q_d, LARCH_V2DI_FTYPE_V2DI_V2DI),
  LSX_BUILTIN (vaddwod_q_d, LARCH_V2DI_FTYPE_V2DI_V2DI),
  LSX_BUILTIN (vaddwev_q_du, LARCH_V2DI_FTYPE_UV2DI_UV2DI),
  LSX_BUILTIN (vaddwod_q_du, LARCH_V2DI_FTYPE_UV2DI_UV2DI),
  LSX_BUILTIN (vsubwev_q_d, LARCH_V2DI_FTYPE_V2DI_V2DI),
  LSX_BUILTIN (vsubwod_q_d, LARCH_V2DI_FTYPE_V2DI_V2DI),
  LSX_BUILTIN (vsubwev_q_du, LARCH_V2DI_FTYPE_UV2DI_UV2DI),
  LSX_BUILTIN (vsubwod_q_du, LARCH_V2DI_FTYPE_UV2DI_UV2DI),
  LSX_BUILTIN (vaddwev_q_du_d, LARCH_V2DI_FTYPE_UV2DI_V2DI),
  LSX_BUILTIN (vaddwod_q_du_d, LARCH_V2DI_FTYPE_UV2DI_V2DI),

  LSX_BUILTIN (vmulwev_d_w, LARCH_V2DI_FTYPE_V4SI_V4SI),
  LSX_BUILTIN (vmulwev_w_h, LARCH_V4SI_FTYPE_V8HI_V8HI),
  LSX_BUILTIN (vmulwev_h_b, LARCH_V8HI_FTYPE_V16QI_V16QI),
  LSX_BUILTIN (vmulwod_d_w, LARCH_V2DI_FTYPE_V4SI_V4SI),
  LSX_BUILTIN (vmulwod_w_h, LARCH_V4SI_FTYPE_V8HI_V8HI),
  LSX_BUILTIN (vmulwod_h_b, LARCH_V8HI_FTYPE_V16QI_V16QI),
  LSX_BUILTIN (vmulwev_d_wu, LARCH_V2DI_FTYPE_UV4SI_UV4SI),
  LSX_BUILTIN (vmulwev_w_hu, LARCH_V4SI_FTYPE_UV8HI_UV8HI),
  LSX_BUILTIN (vmulwev_h_bu, LARCH_V8HI_FTYPE_UV16QI_UV16QI),
  LSX_BUILTIN (vmulwod_d_wu, LARCH_V2DI_FTYPE_UV4SI_UV4SI),
  LSX_BUILTIN (vmulwod_w_hu, LARCH_V4SI_FTYPE_UV8HI_UV8HI),
  LSX_BUILTIN (vmulwod_h_bu, LARCH_V8HI_FTYPE_UV16QI_UV16QI),
  LSX_BUILTIN (vmulwev_d_wu_w, LARCH_V2DI_FTYPE_UV4SI_V4SI),
  LSX_BUILTIN (vmulwev_w_hu_h, LARCH_V4SI_FTYPE_UV8HI_V8HI),
  LSX_BUILTIN (vmulwev_h_bu_b, LARCH_V8HI_FTYPE_UV16QI_V16QI),
  LSX_BUILTIN (vmulwod_d_wu_w, LARCH_V2DI_FTYPE_UV4SI_V4SI),
  LSX_BUILTIN (vmulwod_w_hu_h, LARCH_V4SI_FTYPE_UV8HI_V8HI),
  LSX_BUILTIN (vmulwod_h_bu_b, LARCH_V8HI_FTYPE_UV16QI_V16QI),
  LSX_BUILTIN (vmulwev_q_d, LARCH_V2DI_FTYPE_V2DI_V2DI),
  LSX_BUILTIN (vmulwod_q_d, LARCH_V2DI_FTYPE_V2DI_V2DI),
  LSX_BUILTIN (vmulwev_q_du, LARCH_V2DI_FTYPE_UV2DI_UV2DI),
  LSX_BUILTIN (vmulwod_q_du, LARCH_V2DI_FTYPE_UV2DI_UV2DI),
  LSX_BUILTIN (vmulwev_q_du_d, LARCH_V2DI_FTYPE_UV2DI_V2DI),
  LSX_BUILTIN (vmulwod_q_du_d, LARCH_V2DI_FTYPE_UV2DI_V2DI),
  LSX_BUILTIN (vhaddw_q_d, LARCH_V2DI_FTYPE_V2DI_V2DI),
  LSX_BUILTIN (vhaddw_qu_du, LARCH_UV2DI_FTYPE_UV2DI_UV2DI),
  LSX_BUILTIN (vhsubw_q_d, LARCH_V2DI_FTYPE_V2DI_V2DI),
  LSX_BUILTIN (vhsubw_qu_du, LARCH_UV2DI_FTYPE_UV2DI_UV2DI),
  LSX_BUILTIN (vmaddwev_d_w, LARCH_V2DI_FTYPE_V2DI_V4SI_V4SI),
  LSX_BUILTIN (vmaddwev_w_h, LARCH_V4SI_FTYPE_V4SI_V8HI_V8HI),
  LSX_BUILTIN (vmaddwev_h_b, LARCH_V8HI_FTYPE_V8HI_V16QI_V16QI),
  LSX_BUILTIN (vmaddwev_d_wu, LARCH_UV2DI_FTYPE_UV2DI_UV4SI_UV4SI),
  LSX_BUILTIN (vmaddwev_w_hu, LARCH_UV4SI_FTYPE_UV4SI_UV8HI_UV8HI),
  LSX_BUILTIN (vmaddwev_h_bu, LARCH_UV8HI_FTYPE_UV8HI_UV16QI_UV16QI),
  LSX_BUILTIN (vmaddwod_d_w, LARCH_V2DI_FTYPE_V2DI_V4SI_V4SI),
  LSX_BUILTIN (vmaddwod_w_h, LARCH_V4SI_FTYPE_V4SI_V8HI_V8HI),
  LSX_BUILTIN (vmaddwod_h_b, LARCH_V8HI_FTYPE_V8HI_V16QI_V16QI),
  LSX_BUILTIN (vmaddwod_d_wu, LARCH_UV2DI_FTYPE_UV2DI_UV4SI_UV4SI),
  LSX_BUILTIN (vmaddwod_w_hu, LARCH_UV4SI_FTYPE_UV4SI_UV8HI_UV8HI),
  LSX_BUILTIN (vmaddwod_h_bu, LARCH_UV8HI_FTYPE_UV8HI_UV16QI_UV16QI),
  LSX_BUILTIN (vmaddwev_d_wu_w, LARCH_V2DI_FTYPE_V2DI_UV4SI_V4SI),
  LSX_BUILTIN (vmaddwev_w_hu_h, LARCH_V4SI_FTYPE_V4SI_UV8HI_V8HI),
  LSX_BUILTIN (vmaddwev_h_bu_b, LARCH_V8HI_FTYPE_V8HI_UV16QI_V16QI),
  LSX_BUILTIN (vmaddwod_d_wu_w, LARCH_V2DI_FTYPE_V2DI_UV4SI_V4SI),
  LSX_BUILTIN (vmaddwod_w_hu_h, LARCH_V4SI_FTYPE_V4SI_UV8HI_V8HI),
  LSX_BUILTIN (vmaddwod_h_bu_b, LARCH_V8HI_FTYPE_V8HI_UV16QI_V16QI),
  LSX_BUILTIN (vmaddwev_q_d, LARCH_V2DI_FTYPE_V2DI_V2DI_V2DI),
  LSX_BUILTIN (vmaddwod_q_d, LARCH_V2DI_FTYPE_V2DI_V2DI_V2DI),
  LSX_BUILTIN (vmaddwev_q_du, LARCH_UV2DI_FTYPE_UV2DI_UV2DI_UV2DI),
  LSX_BUILTIN (vmaddwod_q_du, LARCH_UV2DI_FTYPE_UV2DI_UV2DI_UV2DI),
  LSX_BUILTIN (vmaddwev_q_du_d, LARCH_V2DI_FTYPE_V2DI_UV2DI_V2DI),
  LSX_BUILTIN (vmaddwod_q_du_d, LARCH_V2DI_FTYPE_V2DI_UV2DI_V2DI),
  LSX_BUILTIN (vrotr_b, LARCH_V16QI_FTYPE_V16QI_V16QI),
  LSX_BUILTIN (vrotr_h, LARCH_V8HI_FTYPE_V8HI_V8HI),
  LSX_BUILTIN (vrotr_w, LARCH_V4SI_FTYPE_V4SI_V4SI),
  LSX_BUILTIN (vrotr_d, LARCH_V2DI_FTYPE_V2DI_V2DI),
  LSX_BUILTIN (vadd_q, LARCH_V2DI_FTYPE_V2DI_V2DI),
  LSX_BUILTIN (vsub_q, LARCH_V2DI_FTYPE_V2DI_V2DI),
  LSX_BUILTIN (vldrepl_b, LARCH_V16QI_FTYPE_CVPOINTER_SI),
  LSX_BUILTIN (vldrepl_h, LARCH_V8HI_FTYPE_CVPOINTER_SI),
  LSX_BUILTIN (vldrepl_w, LARCH_V4SI_FTYPE_CVPOINTER_SI),
  LSX_BUILTIN (vldrepl_d, LARCH_V2DI_FTYPE_CVPOINTER_SI),

  LSX_BUILTIN (vmskgez_b, LARCH_V16QI_FTYPE_V16QI),
  LSX_BUILTIN (vmsknz_b, LARCH_V16QI_FTYPE_V16QI),
  LSX_BUILTIN (vexth_h_b, LARCH_V8HI_FTYPE_V16QI),
  LSX_BUILTIN (vexth_w_h, LARCH_V4SI_FTYPE_V8HI),
  LSX_BUILTIN (vexth_d_w, LARCH_V2DI_FTYPE_V4SI),
  LSX_BUILTIN (vexth_q_d, LARCH_V2DI_FTYPE_V2DI),
  LSX_BUILTIN (vexth_hu_bu, LARCH_UV8HI_FTYPE_UV16QI),
  LSX_BUILTIN (vexth_wu_hu, LARCH_UV4SI_FTYPE_UV8HI),
  LSX_BUILTIN (vexth_du_wu, LARCH_UV2DI_FTYPE_UV4SI),
  LSX_BUILTIN (vexth_qu_du, LARCH_UV2DI_FTYPE_UV2DI),
  LSX_BUILTIN (vrotri_b, LARCH_V16QI_FTYPE_V16QI_UQI),
  LSX_BUILTIN (vrotri_h, LARCH_V8HI_FTYPE_V8HI_UQI),
  LSX_BUILTIN (vrotri_w, LARCH_V4SI_FTYPE_V4SI_UQI),
  LSX_BUILTIN (vrotri_d, LARCH_V2DI_FTYPE_V2DI_UQI),
  LSX_BUILTIN (vextl_q_d, LARCH_V2DI_FTYPE_V2DI),
  LSX_BUILTIN (vsrlni_b_h, LARCH_V16QI_FTYPE_V16QI_V16QI_USI),
  LSX_BUILTIN (vsrlni_h_w, LARCH_V8HI_FTYPE_V8HI_V8HI_USI),
  LSX_BUILTIN (vsrlni_w_d, LARCH_V4SI_FTYPE_V4SI_V4SI_USI),
  LSX_BUILTIN (vsrlni_d_q, LARCH_V2DI_FTYPE_V2DI_V2DI_USI),
  LSX_BUILTIN (vsrlrni_b_h, LARCH_V16QI_FTYPE_V16QI_V16QI_USI),
  LSX_BUILTIN (vsrlrni_h_w, LARCH_V8HI_FTYPE_V8HI_V8HI_USI),
  LSX_BUILTIN (vsrlrni_w_d, LARCH_V4SI_FTYPE_V4SI_V4SI_USI),
  LSX_BUILTIN (vsrlrni_d_q, LARCH_V2DI_FTYPE_V2DI_V2DI_USI),
  LSX_BUILTIN (vssrlni_b_h, LARCH_V16QI_FTYPE_V16QI_V16QI_USI),
  LSX_BUILTIN (vssrlni_h_w, LARCH_V8HI_FTYPE_V8HI_V8HI_USI),
  LSX_BUILTIN (vssrlni_w_d, LARCH_V4SI_FTYPE_V4SI_V4SI_USI),
  LSX_BUILTIN (vssrlni_d_q, LARCH_V2DI_FTYPE_V2DI_V2DI_USI),
  LSX_BUILTIN (vssrlni_bu_h, LARCH_UV16QI_FTYPE_UV16QI_V16QI_USI),
  LSX_BUILTIN (vssrlni_hu_w, LARCH_UV8HI_FTYPE_UV8HI_V8HI_USI),
  LSX_BUILTIN (vssrlni_wu_d, LARCH_UV4SI_FTYPE_UV4SI_V4SI_USI),
  LSX_BUILTIN (vssrlni_du_q, LARCH_UV2DI_FTYPE_UV2DI_V2DI_USI),
  LSX_BUILTIN (vssrlrni_b_h, LARCH_V16QI_FTYPE_V16QI_V16QI_USI),
  LSX_BUILTIN (vssrlrni_h_w, LARCH_V8HI_FTYPE_V8HI_V8HI_USI),
  LSX_BUILTIN (vssrlrni_w_d, LARCH_V4SI_FTYPE_V4SI_V4SI_USI),
  LSX_BUILTIN (vssrlrni_d_q, LARCH_V2DI_FTYPE_V2DI_V2DI_USI),
  LSX_BUILTIN (vssrlrni_bu_h, LARCH_UV16QI_FTYPE_UV16QI_V16QI_USI),
  LSX_BUILTIN (vssrlrni_hu_w, LARCH_UV8HI_FTYPE_UV8HI_V8HI_USI),
  LSX_BUILTIN (vssrlrni_wu_d, LARCH_UV4SI_FTYPE_UV4SI_V4SI_USI),
  LSX_BUILTIN (vssrlrni_du_q, LARCH_UV2DI_FTYPE_UV2DI_V2DI_USI),
  LSX_BUILTIN (vsrani_b_h, LARCH_V16QI_FTYPE_V16QI_V16QI_USI),
  LSX_BUILTIN (vsrani_h_w, LARCH_V8HI_FTYPE_V8HI_V8HI_USI),
  LSX_BUILTIN (vsrani_w_d, LARCH_V4SI_FTYPE_V4SI_V4SI_USI),
  LSX_BUILTIN (vsrani_d_q, LARCH_V2DI_FTYPE_V2DI_V2DI_USI),
  LSX_BUILTIN (vsrarni_b_h, LARCH_V16QI_FTYPE_V16QI_V16QI_USI),
  LSX_BUILTIN (vsrarni_h_w, LARCH_V8HI_FTYPE_V8HI_V8HI_USI),
  LSX_BUILTIN (vsrarni_w_d, LARCH_V4SI_FTYPE_V4SI_V4SI_USI),
  LSX_BUILTIN (vsrarni_d_q, LARCH_V2DI_FTYPE_V2DI_V2DI_USI),
  LSX_BUILTIN (vssrani_b_h, LARCH_V16QI_FTYPE_V16QI_V16QI_USI),
  LSX_BUILTIN (vssrani_h_w, LARCH_V8HI_FTYPE_V8HI_V8HI_USI),
  LSX_BUILTIN (vssrani_w_d, LARCH_V4SI_FTYPE_V4SI_V4SI_USI),
  LSX_BUILTIN (vssrani_d_q, LARCH_V2DI_FTYPE_V2DI_V2DI_USI),
  LSX_BUILTIN (vssrani_bu_h, LARCH_UV16QI_FTYPE_UV16QI_V16QI_USI),
  LSX_BUILTIN (vssrani_hu_w, LARCH_UV8HI_FTYPE_UV8HI_V8HI_USI),
  LSX_BUILTIN (vssrani_wu_d, LARCH_UV4SI_FTYPE_UV4SI_V4SI_USI),
  LSX_BUILTIN (vssrani_du_q, LARCH_UV2DI_FTYPE_UV2DI_V2DI_USI),
  LSX_BUILTIN (vssrarni_b_h, LARCH_V16QI_FTYPE_V16QI_V16QI_USI),
  LSX_BUILTIN (vssrarni_h_w, LARCH_V8HI_FTYPE_V8HI_V8HI_USI),
  LSX_BUILTIN (vssrarni_w_d, LARCH_V4SI_FTYPE_V4SI_V4SI_USI),
  LSX_BUILTIN (vssrarni_d_q, LARCH_V2DI_FTYPE_V2DI_V2DI_USI),
  LSX_BUILTIN (vssrarni_bu_h, LARCH_UV16QI_FTYPE_UV16QI_V16QI_USI),
  LSX_BUILTIN (vssrarni_hu_w, LARCH_UV8HI_FTYPE_UV8HI_V8HI_USI),
  LSX_BUILTIN (vssrarni_wu_d, LARCH_UV4SI_FTYPE_UV4SI_V4SI_USI),
  LSX_BUILTIN (vssrarni_du_q, LARCH_UV2DI_FTYPE_UV2DI_V2DI_USI),
  LSX_BUILTIN (vpermi_w, LARCH_V4SI_FTYPE_V4SI_V4SI_USI),
  LSX_BUILTIN (vld, LARCH_V16QI_FTYPE_CVPOINTER_SI),
  LSX_NO_TARGET_BUILTIN (vst, LARCH_VOID_FTYPE_V16QI_CVPOINTER_SI),
  LSX_BUILTIN (vssrlrn_b_h, LARCH_V16QI_FTYPE_V8HI_V8HI),
  LSX_BUILTIN (vssrlrn_h_w, LARCH_V8HI_FTYPE_V4SI_V4SI),
  LSX_BUILTIN (vssrlrn_w_d, LARCH_V4SI_FTYPE_V2DI_V2DI),
  LSX_BUILTIN (vssrln_b_h, LARCH_V16QI_FTYPE_V8HI_V8HI),
  LSX_BUILTIN (vssrln_h_w, LARCH_V8HI_FTYPE_V4SI_V4SI),
  LSX_BUILTIN (vssrln_w_d, LARCH_V4SI_FTYPE_V2DI_V2DI),
  LSX_BUILTIN (vorn_v, LARCH_UV16QI_FTYPE_UV16QI_UV16QI),
  LSX_BUILTIN (vldi, LARCH_V2DI_FTYPE_HI),
  LSX_BUILTIN (vshuf_b, LARCH_V16QI_FTYPE_V16QI_V16QI_V16QI),
  LSX_BUILTIN (vldx, LARCH_V16QI_FTYPE_CVPOINTER_DI),
  LSX_NO_TARGET_BUILTIN (vstx, LARCH_VOID_FTYPE_V16QI_CVPOINTER_DI),
  LSX_BUILTIN (vextl_qu_du, LARCH_UV2DI_FTYPE_UV2DI),

  /* Built-in functions for LASX */
  LASX_BUILTIN (xvsll_b, LARCH_V32QI_FTYPE_V32QI_V32QI),
  LASX_BUILTIN (xvsll_h, LARCH_V16HI_FTYPE_V16HI_V16HI),
  LASX_BUILTIN (xvsll_w, LARCH_V8SI_FTYPE_V8SI_V8SI),
  LASX_BUILTIN (xvsll_d, LARCH_V4DI_FTYPE_V4DI_V4DI),
  LASX_BUILTIN (xvslli_b, LARCH_V32QI_FTYPE_V32QI_UQI),
  LASX_BUILTIN (xvslli_h, LARCH_V16HI_FTYPE_V16HI_UQI),
  LASX_BUILTIN (xvslli_w, LARCH_V8SI_FTYPE_V8SI_UQI),
  LASX_BUILTIN (xvslli_d, LARCH_V4DI_FTYPE_V4DI_UQI),
  LASX_BUILTIN (xvsra_b, LARCH_V32QI_FTYPE_V32QI_V32QI),
  LASX_BUILTIN (xvsra_h, LARCH_V16HI_FTYPE_V16HI_V16HI),
  LASX_BUILTIN (xvsra_w, LARCH_V8SI_FTYPE_V8SI_V8SI),
  LASX_BUILTIN (xvsra_d, LARCH_V4DI_FTYPE_V4DI_V4DI),
  LASX_BUILTIN (xvsrai_b, LARCH_V32QI_FTYPE_V32QI_UQI),
  LASX_BUILTIN (xvsrai_h, LARCH_V16HI_FTYPE_V16HI_UQI),
  LASX_BUILTIN (xvsrai_w, LARCH_V8SI_FTYPE_V8SI_UQI),
  LASX_BUILTIN (xvsrai_d, LARCH_V4DI_FTYPE_V4DI_UQI),
  LASX_BUILTIN (xvsrar_b, LARCH_V32QI_FTYPE_V32QI_V32QI),
  LASX_BUILTIN (xvsrar_h, LARCH_V16HI_FTYPE_V16HI_V16HI),
  LASX_BUILTIN (xvsrar_w, LARCH_V8SI_FTYPE_V8SI_V8SI),
  LASX_BUILTIN (xvsrar_d, LARCH_V4DI_FTYPE_V4DI_V4DI),
  LASX_BUILTIN (xvsrari_b, LARCH_V32QI_FTYPE_V32QI_UQI),
  LASX_BUILTIN (xvsrari_h, LARCH_V16HI_FTYPE_V16HI_UQI),
  LASX_BUILTIN (xvsrari_w, LARCH_V8SI_FTYPE_V8SI_UQI),
  LASX_BUILTIN (xvsrari_d, LARCH_V4DI_FTYPE_V4DI_UQI),
  LASX_BUILTIN (xvsrl_b, LARCH_V32QI_FTYPE_V32QI_V32QI),
  LASX_BUILTIN (xvsrl_h, LARCH_V16HI_FTYPE_V16HI_V16HI),
  LASX_BUILTIN (xvsrl_w, LARCH_V8SI_FTYPE_V8SI_V8SI),
  LASX_BUILTIN (xvsrl_d, LARCH_V4DI_FTYPE_V4DI_V4DI),
  LASX_BUILTIN (xvsrli_b, LARCH_V32QI_FTYPE_V32QI_UQI),
  LASX_BUILTIN (xvsrli_h, LARCH_V16HI_FTYPE_V16HI_UQI),
  LASX_BUILTIN (xvsrli_w, LARCH_V8SI_FTYPE_V8SI_UQI),
  LASX_BUILTIN (xvsrli_d, LARCH_V4DI_FTYPE_V4DI_UQI),
  LASX_BUILTIN (xvsrlr_b, LARCH_V32QI_FTYPE_V32QI_V32QI),
  LASX_BUILTIN (xvsrlr_h, LARCH_V16HI_FTYPE_V16HI_V16HI),
  LASX_BUILTIN (xvsrlr_w, LARCH_V8SI_FTYPE_V8SI_V8SI),
  LASX_BUILTIN (xvsrlr_d, LARCH_V4DI_FTYPE_V4DI_V4DI),
  LASX_BUILTIN (xvsrlri_b, LARCH_V32QI_FTYPE_V32QI_UQI),
  LASX_BUILTIN (xvsrlri_h, LARCH_V16HI_FTYPE_V16HI_UQI),
  LASX_BUILTIN (xvsrlri_w, LARCH_V8SI_FTYPE_V8SI_UQI),
  LASX_BUILTIN (xvsrlri_d, LARCH_V4DI_FTYPE_V4DI_UQI),
  LASX_BUILTIN (xvbitclr_b, LARCH_UV32QI_FTYPE_UV32QI_UV32QI),
  LASX_BUILTIN (xvbitclr_h, LARCH_UV16HI_FTYPE_UV16HI_UV16HI),
  LASX_BUILTIN (xvbitclr_w, LARCH_UV8SI_FTYPE_UV8SI_UV8SI),
  LASX_BUILTIN (xvbitclr_d, LARCH_UV4DI_FTYPE_UV4DI_UV4DI),
  LASX_BUILTIN (xvbitclri_b, LARCH_UV32QI_FTYPE_UV32QI_UQI),
  LASX_BUILTIN (xvbitclri_h, LARCH_UV16HI_FTYPE_UV16HI_UQI),
  LASX_BUILTIN (xvbitclri_w, LARCH_UV8SI_FTYPE_UV8SI_UQI),
  LASX_BUILTIN (xvbitclri_d, LARCH_UV4DI_FTYPE_UV4DI_UQI),
  LASX_BUILTIN (xvbitset_b, LARCH_UV32QI_FTYPE_UV32QI_UV32QI),
  LASX_BUILTIN (xvbitset_h, LARCH_UV16HI_FTYPE_UV16HI_UV16HI),
  LASX_BUILTIN (xvbitset_w, LARCH_UV8SI_FTYPE_UV8SI_UV8SI),
  LASX_BUILTIN (xvbitset_d, LARCH_UV4DI_FTYPE_UV4DI_UV4DI),
  LASX_BUILTIN (xvbitseti_b, LARCH_UV32QI_FTYPE_UV32QI_UQI),
  LASX_BUILTIN (xvbitseti_h, LARCH_UV16HI_FTYPE_UV16HI_UQI),
  LASX_BUILTIN (xvbitseti_w, LARCH_UV8SI_FTYPE_UV8SI_UQI),
  LASX_BUILTIN (xvbitseti_d, LARCH_UV4DI_FTYPE_UV4DI_UQI),
  LASX_BUILTIN (xvbitrev_b, LARCH_UV32QI_FTYPE_UV32QI_UV32QI),
  LASX_BUILTIN (xvbitrev_h, LARCH_UV16HI_FTYPE_UV16HI_UV16HI),
  LASX_BUILTIN (xvbitrev_w, LARCH_UV8SI_FTYPE_UV8SI_UV8SI),
  LASX_BUILTIN (xvbitrev_d, LARCH_UV4DI_FTYPE_UV4DI_UV4DI),
  LASX_BUILTIN (xvbitrevi_b, LARCH_UV32QI_FTYPE_UV32QI_UQI),
  LASX_BUILTIN (xvbitrevi_h, LARCH_UV16HI_FTYPE_UV16HI_UQI),
  LASX_BUILTIN (xvbitrevi_w, LARCH_UV8SI_FTYPE_UV8SI_UQI),
  LASX_BUILTIN (xvbitrevi_d, LARCH_UV4DI_FTYPE_UV4DI_UQI),
  LASX_BUILTIN (xvadd_b, LARCH_V32QI_FTYPE_V32QI_V32QI),
  LASX_BUILTIN (xvadd_h, LARCH_V16HI_FTYPE_V16HI_V16HI),
  LASX_BUILTIN (xvadd_w, LARCH_V8SI_FTYPE_V8SI_V8SI),
  LASX_BUILTIN (xvadd_d, LARCH_V4DI_FTYPE_V4DI_V4DI),
  LASX_BUILTIN (xvaddi_bu, LARCH_V32QI_FTYPE_V32QI_UQI),
  LASX_BUILTIN (xvaddi_hu, LARCH_V16HI_FTYPE_V16HI_UQI),
  LASX_BUILTIN (xvaddi_wu, LARCH_V8SI_FTYPE_V8SI_UQI),
  LASX_BUILTIN (xvaddi_du, LARCH_V4DI_FTYPE_V4DI_UQI),
  LASX_BUILTIN (xvsub_b, LARCH_V32QI_FTYPE_V32QI_V32QI),
  LASX_BUILTIN (xvsub_h, LARCH_V16HI_FTYPE_V16HI_V16HI),
  LASX_BUILTIN (xvsub_w, LARCH_V8SI_FTYPE_V8SI_V8SI),
  LASX_BUILTIN (xvsub_d, LARCH_V4DI_FTYPE_V4DI_V4DI),
  LASX_BUILTIN (xvsubi_bu, LARCH_V32QI_FTYPE_V32QI_UQI),
  LASX_BUILTIN (xvsubi_hu, LARCH_V16HI_FTYPE_V16HI_UQI),
  LASX_BUILTIN (xvsubi_wu, LARCH_V8SI_FTYPE_V8SI_UQI),
  LASX_BUILTIN (xvsubi_du, LARCH_V4DI_FTYPE_V4DI_UQI),
  LASX_BUILTIN (xvmax_b, LARCH_V32QI_FTYPE_V32QI_V32QI),
  LASX_BUILTIN (xvmax_h, LARCH_V16HI_FTYPE_V16HI_V16HI),
  LASX_BUILTIN (xvmax_w, LARCH_V8SI_FTYPE_V8SI_V8SI),
  LASX_BUILTIN (xvmax_d, LARCH_V4DI_FTYPE_V4DI_V4DI),
  LASX_BUILTIN (xvmaxi_b, LARCH_V32QI_FTYPE_V32QI_QI),
  LASX_BUILTIN (xvmaxi_h, LARCH_V16HI_FTYPE_V16HI_QI),
  LASX_BUILTIN (xvmaxi_w, LARCH_V8SI_FTYPE_V8SI_QI),
  LASX_BUILTIN (xvmaxi_d, LARCH_V4DI_FTYPE_V4DI_QI),
  LASX_BUILTIN (xvmax_bu, LARCH_UV32QI_FTYPE_UV32QI_UV32QI),
  LASX_BUILTIN (xvmax_hu, LARCH_UV16HI_FTYPE_UV16HI_UV16HI),
  LASX_BUILTIN (xvmax_wu, LARCH_UV8SI_FTYPE_UV8SI_UV8SI),
  LASX_BUILTIN (xvmax_du, LARCH_UV4DI_FTYPE_UV4DI_UV4DI),
  LASX_BUILTIN (xvmaxi_bu, LARCH_UV32QI_FTYPE_UV32QI_UQI),
  LASX_BUILTIN (xvmaxi_hu, LARCH_UV16HI_FTYPE_UV16HI_UQI),
  LASX_BUILTIN (xvmaxi_wu, LARCH_UV8SI_FTYPE_UV8SI_UQI),
  LASX_BUILTIN (xvmaxi_du, LARCH_UV4DI_FTYPE_UV4DI_UQI),
  LASX_BUILTIN (xvmin_b, LARCH_V32QI_FTYPE_V32QI_V32QI),
  LASX_BUILTIN (xvmin_h, LARCH_V16HI_FTYPE_V16HI_V16HI),
  LASX_BUILTIN (xvmin_w, LARCH_V8SI_FTYPE_V8SI_V8SI),
  LASX_BUILTIN (xvmin_d, LARCH_V4DI_FTYPE_V4DI_V4DI),
  LASX_BUILTIN (xvmini_b, LARCH_V32QI_FTYPE_V32QI_QI),
  LASX_BUILTIN (xvmini_h, LARCH_V16HI_FTYPE_V16HI_QI),
  LASX_BUILTIN (xvmini_w, LARCH_V8SI_FTYPE_V8SI_QI),
  LASX_BUILTIN (xvmini_d, LARCH_V4DI_FTYPE_V4DI_QI),
  LASX_BUILTIN (xvmin_bu, LARCH_UV32QI_FTYPE_UV32QI_UV32QI),
  LASX_BUILTIN (xvmin_hu, LARCH_UV16HI_FTYPE_UV16HI_UV16HI),
  LASX_BUILTIN (xvmin_wu, LARCH_UV8SI_FTYPE_UV8SI_UV8SI),
  LASX_BUILTIN (xvmin_du, LARCH_UV4DI_FTYPE_UV4DI_UV4DI),
  LASX_BUILTIN (xvmini_bu, LARCH_UV32QI_FTYPE_UV32QI_UQI),
  LASX_BUILTIN (xvmini_hu, LARCH_UV16HI_FTYPE_UV16HI_UQI),
  LASX_BUILTIN (xvmini_wu, LARCH_UV8SI_FTYPE_UV8SI_UQI),
  LASX_BUILTIN (xvmini_du, LARCH_UV4DI_FTYPE_UV4DI_UQI),
  LASX_BUILTIN (xvseq_b, LARCH_V32QI_FTYPE_V32QI_V32QI),
  LASX_BUILTIN (xvseq_h, LARCH_V16HI_FTYPE_V16HI_V16HI),
  LASX_BUILTIN (xvseq_w, LARCH_V8SI_FTYPE_V8SI_V8SI),
  LASX_BUILTIN (xvseq_d, LARCH_V4DI_FTYPE_V4DI_V4DI),
  LASX_BUILTIN (xvseqi_b, LARCH_V32QI_FTYPE_V32QI_QI),
  LASX_BUILTIN (xvseqi_h, LARCH_V16HI_FTYPE_V16HI_QI),
  LASX_BUILTIN (xvseqi_w, LARCH_V8SI_FTYPE_V8SI_QI),
  LASX_BUILTIN (xvseqi_d, LARCH_V4DI_FTYPE_V4DI_QI),
  LASX_BUILTIN (xvslt_b, LARCH_V32QI_FTYPE_V32QI_V32QI),
  LASX_BUILTIN (xvslt_h, LARCH_V16HI_FTYPE_V16HI_V16HI),
  LASX_BUILTIN (xvslt_w, LARCH_V8SI_FTYPE_V8SI_V8SI),
  LASX_BUILTIN (xvslt_d, LARCH_V4DI_FTYPE_V4DI_V4DI),
  LASX_BUILTIN (xvslti_b, LARCH_V32QI_FTYPE_V32QI_QI),
  LASX_BUILTIN (xvslti_h, LARCH_V16HI_FTYPE_V16HI_QI),
  LASX_BUILTIN (xvslti_w, LARCH_V8SI_FTYPE_V8SI_QI),
  LASX_BUILTIN (xvslti_d, LARCH_V4DI_FTYPE_V4DI_QI),
  LASX_BUILTIN (xvslt_bu, LARCH_V32QI_FTYPE_UV32QI_UV32QI),
  LASX_BUILTIN (xvslt_hu, LARCH_V16HI_FTYPE_UV16HI_UV16HI),
  LASX_BUILTIN (xvslt_wu, LARCH_V8SI_FTYPE_UV8SI_UV8SI),
  LASX_BUILTIN (xvslt_du, LARCH_V4DI_FTYPE_UV4DI_UV4DI),
  LASX_BUILTIN (xvslti_bu, LARCH_V32QI_FTYPE_UV32QI_UQI),
  LASX_BUILTIN (xvslti_hu, LARCH_V16HI_FTYPE_UV16HI_UQI),
  LASX_BUILTIN (xvslti_wu, LARCH_V8SI_FTYPE_UV8SI_UQI),
  LASX_BUILTIN (xvslti_du, LARCH_V4DI_FTYPE_UV4DI_UQI),
  LASX_BUILTIN (xvsle_b, LARCH_V32QI_FTYPE_V32QI_V32QI),
  LASX_BUILTIN (xvsle_h, LARCH_V16HI_FTYPE_V16HI_V16HI),
  LASX_BUILTIN (xvsle_w, LARCH_V8SI_FTYPE_V8SI_V8SI),
  LASX_BUILTIN (xvsle_d, LARCH_V4DI_FTYPE_V4DI_V4DI),
  LASX_BUILTIN (xvslei_b, LARCH_V32QI_FTYPE_V32QI_QI),
  LASX_BUILTIN (xvslei_h, LARCH_V16HI_FTYPE_V16HI_QI),
  LASX_BUILTIN (xvslei_w, LARCH_V8SI_FTYPE_V8SI_QI),
  LASX_BUILTIN (xvslei_d, LARCH_V4DI_FTYPE_V4DI_QI),
  LASX_BUILTIN (xvsle_bu, LARCH_V32QI_FTYPE_UV32QI_UV32QI),
  LASX_BUILTIN (xvsle_hu, LARCH_V16HI_FTYPE_UV16HI_UV16HI),
  LASX_BUILTIN (xvsle_wu, LARCH_V8SI_FTYPE_UV8SI_UV8SI),
  LASX_BUILTIN (xvsle_du, LARCH_V4DI_FTYPE_UV4DI_UV4DI),
  LASX_BUILTIN (xvslei_bu, LARCH_V32QI_FTYPE_UV32QI_UQI),
  LASX_BUILTIN (xvslei_hu, LARCH_V16HI_FTYPE_UV16HI_UQI),
  LASX_BUILTIN (xvslei_wu, LARCH_V8SI_FTYPE_UV8SI_UQI),
  LASX_BUILTIN (xvslei_du, LARCH_V4DI_FTYPE_UV4DI_UQI),

  LASX_BUILTIN (xvsat_b, LARCH_V32QI_FTYPE_V32QI_UQI),
  LASX_BUILTIN (xvsat_h, LARCH_V16HI_FTYPE_V16HI_UQI),
  LASX_BUILTIN (xvsat_w, LARCH_V8SI_FTYPE_V8SI_UQI),
  LASX_BUILTIN (xvsat_d, LARCH_V4DI_FTYPE_V4DI_UQI),
  LASX_BUILTIN (xvsat_bu, LARCH_UV32QI_FTYPE_UV32QI_UQI),
  LASX_BUILTIN (xvsat_hu, LARCH_UV16HI_FTYPE_UV16HI_UQI),
  LASX_BUILTIN (xvsat_wu, LARCH_UV8SI_FTYPE_UV8SI_UQI),
  LASX_BUILTIN (xvsat_du, LARCH_UV4DI_FTYPE_UV4DI_UQI),

  LASX_BUILTIN (xvadda_b, LARCH_V32QI_FTYPE_V32QI_V32QI),
  LASX_BUILTIN (xvadda_h, LARCH_V16HI_FTYPE_V16HI_V16HI),
  LASX_BUILTIN (xvadda_w, LARCH_V8SI_FTYPE_V8SI_V8SI),
  LASX_BUILTIN (xvadda_d, LARCH_V4DI_FTYPE_V4DI_V4DI),
  LASX_BUILTIN (xvsadd_b, LARCH_V32QI_FTYPE_V32QI_V32QI),
  LASX_BUILTIN (xvsadd_h, LARCH_V16HI_FTYPE_V16HI_V16HI),
  LASX_BUILTIN (xvsadd_w, LARCH_V8SI_FTYPE_V8SI_V8SI),
  LASX_BUILTIN (xvsadd_d, LARCH_V4DI_FTYPE_V4DI_V4DI),
  LASX_BUILTIN (xvsadd_bu, LARCH_UV32QI_FTYPE_UV32QI_UV32QI),
  LASX_BUILTIN (xvsadd_hu, LARCH_UV16HI_FTYPE_UV16HI_UV16HI),
  LASX_BUILTIN (xvsadd_wu, LARCH_UV8SI_FTYPE_UV8SI_UV8SI),
  LASX_BUILTIN (xvsadd_du, LARCH_UV4DI_FTYPE_UV4DI_UV4DI),

  LASX_BUILTIN (xvavg_b, LARCH_V32QI_FTYPE_V32QI_V32QI),
  LASX_BUILTIN (xvavg_h, LARCH_V16HI_FTYPE_V16HI_V16HI),
  LASX_BUILTIN (xvavg_w, LARCH_V8SI_FTYPE_V8SI_V8SI),
  LASX_BUILTIN (xvavg_d, LARCH_V4DI_FTYPE_V4DI_V4DI),
  LASX_BUILTIN (xvavg_bu, LARCH_UV32QI_FTYPE_UV32QI_UV32QI),
  LASX_BUILTIN (xvavg_hu, LARCH_UV16HI_FTYPE_UV16HI_UV16HI),
  LASX_BUILTIN (xvavg_wu, LARCH_UV8SI_FTYPE_UV8SI_UV8SI),
  LASX_BUILTIN (xvavg_du, LARCH_UV4DI_FTYPE_UV4DI_UV4DI),

  LASX_BUILTIN (xvavgr_b, LARCH_V32QI_FTYPE_V32QI_V32QI),
  LASX_BUILTIN (xvavgr_h, LARCH_V16HI_FTYPE_V16HI_V16HI),
  LASX_BUILTIN (xvavgr_w, LARCH_V8SI_FTYPE_V8SI_V8SI),
  LASX_BUILTIN (xvavgr_d, LARCH_V4DI_FTYPE_V4DI_V4DI),
  LASX_BUILTIN (xvavgr_bu, LARCH_UV32QI_FTYPE_UV32QI_UV32QI),
  LASX_BUILTIN (xvavgr_hu, LARCH_UV16HI_FTYPE_UV16HI_UV16HI),
  LASX_BUILTIN (xvavgr_wu, LARCH_UV8SI_FTYPE_UV8SI_UV8SI),
  LASX_BUILTIN (xvavgr_du, LARCH_UV4DI_FTYPE_UV4DI_UV4DI),

  LASX_BUILTIN (xvssub_b, LARCH_V32QI_FTYPE_V32QI_V32QI),
  LASX_BUILTIN (xvssub_h, LARCH_V16HI_FTYPE_V16HI_V16HI),
  LASX_BUILTIN (xvssub_w, LARCH_V8SI_FTYPE_V8SI_V8SI),
  LASX_BUILTIN (xvssub_d, LARCH_V4DI_FTYPE_V4DI_V4DI),
  LASX_BUILTIN (xvssub_bu, LARCH_UV32QI_FTYPE_UV32QI_UV32QI),
  LASX_BUILTIN (xvssub_hu, LARCH_UV16HI_FTYPE_UV16HI_UV16HI),
  LASX_BUILTIN (xvssub_wu, LARCH_UV8SI_FTYPE_UV8SI_UV8SI),
  LASX_BUILTIN (xvssub_du, LARCH_UV4DI_FTYPE_UV4DI_UV4DI),
  LASX_BUILTIN (xvabsd_b, LARCH_V32QI_FTYPE_V32QI_V32QI),
  LASX_BUILTIN (xvabsd_h, LARCH_V16HI_FTYPE_V16HI_V16HI),
  LASX_BUILTIN (xvabsd_w, LARCH_V8SI_FTYPE_V8SI_V8SI),
  LASX_BUILTIN (xvabsd_d, LARCH_V4DI_FTYPE_V4DI_V4DI),
  LASX_BUILTIN (xvabsd_bu, LARCH_UV32QI_FTYPE_UV32QI_UV32QI),
  LASX_BUILTIN (xvabsd_hu, LARCH_UV16HI_FTYPE_UV16HI_UV16HI),
  LASX_BUILTIN (xvabsd_wu, LARCH_UV8SI_FTYPE_UV8SI_UV8SI),
  LASX_BUILTIN (xvabsd_du, LARCH_UV4DI_FTYPE_UV4DI_UV4DI),

  LASX_BUILTIN (xvmul_b, LARCH_V32QI_FTYPE_V32QI_V32QI),
  LASX_BUILTIN (xvmul_h, LARCH_V16HI_FTYPE_V16HI_V16HI),
  LASX_BUILTIN (xvmul_w, LARCH_V8SI_FTYPE_V8SI_V8SI),
  LASX_BUILTIN (xvmul_d, LARCH_V4DI_FTYPE_V4DI_V4DI),
  LASX_BUILTIN (xvmadd_b, LARCH_V32QI_FTYPE_V32QI_V32QI_V32QI),
  LASX_BUILTIN (xvmadd_h, LARCH_V16HI_FTYPE_V16HI_V16HI_V16HI),
  LASX_BUILTIN (xvmadd_w, LARCH_V8SI_FTYPE_V8SI_V8SI_V8SI),
  LASX_BUILTIN (xvmadd_d, LARCH_V4DI_FTYPE_V4DI_V4DI_V4DI),
  LASX_BUILTIN (xvmsub_b, LARCH_V32QI_FTYPE_V32QI_V32QI_V32QI),
  LASX_BUILTIN (xvmsub_h, LARCH_V16HI_FTYPE_V16HI_V16HI_V16HI),
  LASX_BUILTIN (xvmsub_w, LARCH_V8SI_FTYPE_V8SI_V8SI_V8SI),
  LASX_BUILTIN (xvmsub_d, LARCH_V4DI_FTYPE_V4DI_V4DI_V4DI),
  LASX_BUILTIN (xvdiv_b, LARCH_V32QI_FTYPE_V32QI_V32QI),
  LASX_BUILTIN (xvdiv_h, LARCH_V16HI_FTYPE_V16HI_V16HI),
  LASX_BUILTIN (xvdiv_w, LARCH_V8SI_FTYPE_V8SI_V8SI),
  LASX_BUILTIN (xvdiv_d, LARCH_V4DI_FTYPE_V4DI_V4DI),
  LASX_BUILTIN (xvdiv_bu, LARCH_UV32QI_FTYPE_UV32QI_UV32QI),
  LASX_BUILTIN (xvdiv_hu, LARCH_UV16HI_FTYPE_UV16HI_UV16HI),
  LASX_BUILTIN (xvdiv_wu, LARCH_UV8SI_FTYPE_UV8SI_UV8SI),
  LASX_BUILTIN (xvdiv_du, LARCH_UV4DI_FTYPE_UV4DI_UV4DI),
  LASX_BUILTIN (xvhaddw_h_b, LARCH_V16HI_FTYPE_V32QI_V32QI),
  LASX_BUILTIN (xvhaddw_w_h, LARCH_V8SI_FTYPE_V16HI_V16HI),
  LASX_BUILTIN (xvhaddw_d_w, LARCH_V4DI_FTYPE_V8SI_V8SI),
  LASX_BUILTIN (xvhaddw_hu_bu, LARCH_UV16HI_FTYPE_UV32QI_UV32QI),
  LASX_BUILTIN (xvhaddw_wu_hu, LARCH_UV8SI_FTYPE_UV16HI_UV16HI),
  LASX_BUILTIN (xvhaddw_du_wu, LARCH_UV4DI_FTYPE_UV8SI_UV8SI),
  LASX_BUILTIN (xvhsubw_h_b, LARCH_V16HI_FTYPE_V32QI_V32QI),
  LASX_BUILTIN (xvhsubw_w_h, LARCH_V8SI_FTYPE_V16HI_V16HI),
  LASX_BUILTIN (xvhsubw_d_w, LARCH_V4DI_FTYPE_V8SI_V8SI),
  LASX_BUILTIN (xvhsubw_hu_bu, LARCH_V16HI_FTYPE_UV32QI_UV32QI),
  LASX_BUILTIN (xvhsubw_wu_hu, LARCH_V8SI_FTYPE_UV16HI_UV16HI),
  LASX_BUILTIN (xvhsubw_du_wu, LARCH_V4DI_FTYPE_UV8SI_UV8SI),
  LASX_BUILTIN (xvmod_b, LARCH_V32QI_FTYPE_V32QI_V32QI),
  LASX_BUILTIN (xvmod_h, LARCH_V16HI_FTYPE_V16HI_V16HI),
  LASX_BUILTIN (xvmod_w, LARCH_V8SI_FTYPE_V8SI_V8SI),
  LASX_BUILTIN (xvmod_d, LARCH_V4DI_FTYPE_V4DI_V4DI),
  LASX_BUILTIN (xvmod_bu, LARCH_UV32QI_FTYPE_UV32QI_UV32QI),
  LASX_BUILTIN (xvmod_hu, LARCH_UV16HI_FTYPE_UV16HI_UV16HI),
  LASX_BUILTIN (xvmod_wu, LARCH_UV8SI_FTYPE_UV8SI_UV8SI),
  LASX_BUILTIN (xvmod_du, LARCH_UV4DI_FTYPE_UV4DI_UV4DI),

  LASX_BUILTIN (xvrepl128vei_b, LARCH_V32QI_FTYPE_V32QI_UQI),
  LASX_BUILTIN (xvrepl128vei_h, LARCH_V16HI_FTYPE_V16HI_UQI),
  LASX_BUILTIN (xvrepl128vei_w, LARCH_V8SI_FTYPE_V8SI_UQI),
  LASX_BUILTIN (xvrepl128vei_d, LARCH_V4DI_FTYPE_V4DI_UQI),
  LASX_BUILTIN (xvpickev_b, LARCH_V32QI_FTYPE_V32QI_V32QI),
  LASX_BUILTIN (xvpickev_h, LARCH_V16HI_FTYPE_V16HI_V16HI),
  LASX_BUILTIN (xvpickev_w, LARCH_V8SI_FTYPE_V8SI_V8SI),
  LASX_BUILTIN (xvpickev_d, LARCH_V4DI_FTYPE_V4DI_V4DI),
  LASX_BUILTIN (xvpickod_b, LARCH_V32QI_FTYPE_V32QI_V32QI),
  LASX_BUILTIN (xvpickod_h, LARCH_V16HI_FTYPE_V16HI_V16HI),
  LASX_BUILTIN (xvpickod_w, LARCH_V8SI_FTYPE_V8SI_V8SI),
  LASX_BUILTIN (xvpickod_d, LARCH_V4DI_FTYPE_V4DI_V4DI),
  LASX_BUILTIN (xvilvh_b, LARCH_V32QI_FTYPE_V32QI_V32QI),
  LASX_BUILTIN (xvilvh_h, LARCH_V16HI_FTYPE_V16HI_V16HI),
  LASX_BUILTIN (xvilvh_w, LARCH_V8SI_FTYPE_V8SI_V8SI),
  LASX_BUILTIN (xvilvh_d, LARCH_V4DI_FTYPE_V4DI_V4DI),
  LASX_BUILTIN (xvilvl_b, LARCH_V32QI_FTYPE_V32QI_V32QI),
  LASX_BUILTIN (xvilvl_h, LARCH_V16HI_FTYPE_V16HI_V16HI),
  LASX_BUILTIN (xvilvl_w, LARCH_V8SI_FTYPE_V8SI_V8SI),
  LASX_BUILTIN (xvilvl_d, LARCH_V4DI_FTYPE_V4DI_V4DI),
  LASX_BUILTIN (xvpackev_b, LARCH_V32QI_FTYPE_V32QI_V32QI),
  LASX_BUILTIN (xvpackev_h, LARCH_V16HI_FTYPE_V16HI_V16HI),
  LASX_BUILTIN (xvpackev_w, LARCH_V8SI_FTYPE_V8SI_V8SI),
  LASX_BUILTIN (xvpackev_d, LARCH_V4DI_FTYPE_V4DI_V4DI),
  LASX_BUILTIN (xvpackod_b, LARCH_V32QI_FTYPE_V32QI_V32QI),
  LASX_BUILTIN (xvpackod_h, LARCH_V16HI_FTYPE_V16HI_V16HI),
  LASX_BUILTIN (xvpackod_w, LARCH_V8SI_FTYPE_V8SI_V8SI),
  LASX_BUILTIN (xvpackod_d, LARCH_V4DI_FTYPE_V4DI_V4DI),
  LASX_BUILTIN (xvshuf_b, LARCH_V32QI_FTYPE_V32QI_V32QI_V32QI),
  LASX_BUILTIN (xvshuf_h, LARCH_V16HI_FTYPE_V16HI_V16HI_V16HI),
  LASX_BUILTIN (xvshuf_w, LARCH_V8SI_FTYPE_V8SI_V8SI_V8SI),
  LASX_BUILTIN (xvshuf_d, LARCH_V4DI_FTYPE_V4DI_V4DI_V4DI),
  LASX_BUILTIN (xvand_v, LARCH_UV32QI_FTYPE_UV32QI_UV32QI),
  LASX_BUILTIN (xvandi_b, LARCH_UV32QI_FTYPE_UV32QI_UQI),
  LASX_BUILTIN (xvor_v, LARCH_UV32QI_FTYPE_UV32QI_UV32QI),
  LASX_BUILTIN (xvori_b, LARCH_UV32QI_FTYPE_UV32QI_UQI),
  LASX_BUILTIN (xvnor_v, LARCH_UV32QI_FTYPE_UV32QI_UV32QI),
  LASX_BUILTIN (xvnori_b, LARCH_UV32QI_FTYPE_UV32QI_UQI),
  LASX_BUILTIN (xvxor_v, LARCH_UV32QI_FTYPE_UV32QI_UV32QI),
  LASX_BUILTIN (xvxori_b, LARCH_UV32QI_FTYPE_UV32QI_UQI),
  LASX_BUILTIN (xvbitsel_v, LARCH_UV32QI_FTYPE_UV32QI_UV32QI_UV32QI),
  LASX_BUILTIN (xvbitseli_b, LARCH_UV32QI_FTYPE_UV32QI_UV32QI_USI),

  LASX_BUILTIN (xvshuf4i_b, LARCH_V32QI_FTYPE_V32QI_USI),
  LASX_BUILTIN (xvshuf4i_h, LARCH_V16HI_FTYPE_V16HI_USI),
  LASX_BUILTIN (xvshuf4i_w, LARCH_V8SI_FTYPE_V8SI_USI),

  LASX_BUILTIN (xvreplgr2vr_b, LARCH_V32QI_FTYPE_SI),
  LASX_BUILTIN (xvreplgr2vr_h, LARCH_V16HI_FTYPE_SI),
  LASX_BUILTIN (xvreplgr2vr_w, LARCH_V8SI_FTYPE_SI),
  LASX_BUILTIN (xvreplgr2vr_d, LARCH_V4DI_FTYPE_DI),
  LASX_BUILTIN (xvpcnt_b, LARCH_V32QI_FTYPE_V32QI),
  LASX_BUILTIN (xvpcnt_h, LARCH_V16HI_FTYPE_V16HI),
  LASX_BUILTIN (xvpcnt_w, LARCH_V8SI_FTYPE_V8SI),
  LASX_BUILTIN (xvpcnt_d, LARCH_V4DI_FTYPE_V4DI),
  LASX_BUILTIN (xvclo_b, LARCH_V32QI_FTYPE_V32QI),
  LASX_BUILTIN (xvclo_h, LARCH_V16HI_FTYPE_V16HI),
  LASX_BUILTIN (xvclo_w, LARCH_V8SI_FTYPE_V8SI),
  LASX_BUILTIN (xvclo_d, LARCH_V4DI_FTYPE_V4DI),
  LASX_BUILTIN (xvclz_b, LARCH_V32QI_FTYPE_V32QI),
  LASX_BUILTIN (xvclz_h, LARCH_V16HI_FTYPE_V16HI),
  LASX_BUILTIN (xvclz_w, LARCH_V8SI_FTYPE_V8SI),
  LASX_BUILTIN (xvclz_d, LARCH_V4DI_FTYPE_V4DI),

  LASX_BUILTIN (xvrepli_b, LARCH_V32QI_FTYPE_HI),
  LASX_BUILTIN (xvrepli_h, LARCH_V16HI_FTYPE_HI),
  LASX_BUILTIN (xvrepli_w, LARCH_V8SI_FTYPE_HI),
  LASX_BUILTIN (xvrepli_d, LARCH_V4DI_FTYPE_HI),
  LASX_BUILTIN (xvfcmp_caf_s, LARCH_V8SI_FTYPE_V8SF_V8SF),
  LASX_BUILTIN (xvfcmp_caf_d, LARCH_V4DI_FTYPE_V4DF_V4DF),
  LASX_BUILTIN (xvfcmp_cor_s, LARCH_V8SI_FTYPE_V8SF_V8SF),
  LASX_BUILTIN (xvfcmp_cor_d, LARCH_V4DI_FTYPE_V4DF_V4DF),
  LASX_BUILTIN (xvfcmp_cun_s, LARCH_V8SI_FTYPE_V8SF_V8SF),
  LASX_BUILTIN (xvfcmp_cun_d, LARCH_V4DI_FTYPE_V4DF_V4DF),
  LASX_BUILTIN (xvfcmp_cune_s, LARCH_V8SI_FTYPE_V8SF_V8SF),
  LASX_BUILTIN (xvfcmp_cune_d, LARCH_V4DI_FTYPE_V4DF_V4DF),
  LASX_BUILTIN (xvfcmp_cueq_s, LARCH_V8SI_FTYPE_V8SF_V8SF),
  LASX_BUILTIN (xvfcmp_cueq_d, LARCH_V4DI_FTYPE_V4DF_V4DF),
  LASX_BUILTIN (xvfcmp_ceq_s, LARCH_V8SI_FTYPE_V8SF_V8SF),
  LASX_BUILTIN (xvfcmp_ceq_d, LARCH_V4DI_FTYPE_V4DF_V4DF),
  LASX_BUILTIN (xvfcmp_cne_s, LARCH_V8SI_FTYPE_V8SF_V8SF),
  LASX_BUILTIN (xvfcmp_cne_d, LARCH_V4DI_FTYPE_V4DF_V4DF),
  LASX_BUILTIN (xvfcmp_clt_s, LARCH_V8SI_FTYPE_V8SF_V8SF),
  LASX_BUILTIN (xvfcmp_clt_d, LARCH_V4DI_FTYPE_V4DF_V4DF),
  LASX_BUILTIN (xvfcmp_cult_s, LARCH_V8SI_FTYPE_V8SF_V8SF),
  LASX_BUILTIN (xvfcmp_cult_d, LARCH_V4DI_FTYPE_V4DF_V4DF),
  LASX_BUILTIN (xvfcmp_cle_s, LARCH_V8SI_FTYPE_V8SF_V8SF),
  LASX_BUILTIN (xvfcmp_cle_d, LARCH_V4DI_FTYPE_V4DF_V4DF),
  LASX_BUILTIN (xvfcmp_cule_s, LARCH_V8SI_FTYPE_V8SF_V8SF),
  LASX_BUILTIN (xvfcmp_cule_d, LARCH_V4DI_FTYPE_V4DF_V4DF),
  LASX_BUILTIN (xvfcmp_saf_s, LARCH_V8SI_FTYPE_V8SF_V8SF),
  LASX_BUILTIN (xvfcmp_saf_d, LARCH_V4DI_FTYPE_V4DF_V4DF),
  LASX_BUILTIN (xvfcmp_sor_s, LARCH_V8SI_FTYPE_V8SF_V8SF),
  LASX_BUILTIN (xvfcmp_sor_d, LARCH_V4DI_FTYPE_V4DF_V4DF),
  LASX_BUILTIN (xvfcmp_sun_s, LARCH_V8SI_FTYPE_V8SF_V8SF),
  LASX_BUILTIN (xvfcmp_sun_d, LARCH_V4DI_FTYPE_V4DF_V4DF),
  LASX_BUILTIN (xvfcmp_sune_s, LARCH_V8SI_FTYPE_V8SF_V8SF),
  LASX_BUILTIN (xvfcmp_sune_d, LARCH_V4DI_FTYPE_V4DF_V4DF),
  LASX_BUILTIN (xvfcmp_sueq_s, LARCH_V8SI_FTYPE_V8SF_V8SF),
  LASX_BUILTIN (xvfcmp_sueq_d, LARCH_V4DI_FTYPE_V4DF_V4DF),
  LASX_BUILTIN (xvfcmp_seq_s, LARCH_V8SI_FTYPE_V8SF_V8SF),
  LASX_BUILTIN (xvfcmp_seq_d, LARCH_V4DI_FTYPE_V4DF_V4DF),
  LASX_BUILTIN (xvfcmp_sne_s, LARCH_V8SI_FTYPE_V8SF_V8SF),
  LASX_BUILTIN (xvfcmp_sne_d, LARCH_V4DI_FTYPE_V4DF_V4DF),
  LASX_BUILTIN (xvfcmp_slt_s, LARCH_V8SI_FTYPE_V8SF_V8SF),
  LASX_BUILTIN (xvfcmp_slt_d, LARCH_V4DI_FTYPE_V4DF_V4DF),
  LASX_BUILTIN (xvfcmp_sult_s, LARCH_V8SI_FTYPE_V8SF_V8SF),
  LASX_BUILTIN (xvfcmp_sult_d, LARCH_V4DI_FTYPE_V4DF_V4DF),
  LASX_BUILTIN (xvfcmp_sle_s, LARCH_V8SI_FTYPE_V8SF_V8SF),
  LASX_BUILTIN (xvfcmp_sle_d, LARCH_V4DI_FTYPE_V4DF_V4DF),
  LASX_BUILTIN (xvfcmp_sule_s, LARCH_V8SI_FTYPE_V8SF_V8SF),
  LASX_BUILTIN (xvfcmp_sule_d, LARCH_V4DI_FTYPE_V4DF_V4DF),
  LASX_BUILTIN (xvfadd_s, LARCH_V8SF_FTYPE_V8SF_V8SF),
  LASX_BUILTIN (xvfadd_d, LARCH_V4DF_FTYPE_V4DF_V4DF),
  LASX_BUILTIN (xvfsub_s, LARCH_V8SF_FTYPE_V8SF_V8SF),
  LASX_BUILTIN (xvfsub_d, LARCH_V4DF_FTYPE_V4DF_V4DF),
  LASX_BUILTIN (xvfmul_s, LARCH_V8SF_FTYPE_V8SF_V8SF),
  LASX_BUILTIN (xvfmul_d, LARCH_V4DF_FTYPE_V4DF_V4DF),
  LASX_BUILTIN (xvfdiv_s, LARCH_V8SF_FTYPE_V8SF_V8SF),
  LASX_BUILTIN (xvfdiv_d, LARCH_V4DF_FTYPE_V4DF_V4DF),
  LASX_BUILTIN (xvfcvt_h_s, LARCH_V16HI_FTYPE_V8SF_V8SF),
  LASX_BUILTIN (xvfcvt_s_d, LARCH_V8SF_FTYPE_V4DF_V4DF),
  LASX_BUILTIN (xvfmin_s, LARCH_V8SF_FTYPE_V8SF_V8SF),
  LASX_BUILTIN (xvfmin_d, LARCH_V4DF_FTYPE_V4DF_V4DF),
  LASX_BUILTIN (xvfmina_s, LARCH_V8SF_FTYPE_V8SF_V8SF),
  LASX_BUILTIN (xvfmina_d, LARCH_V4DF_FTYPE_V4DF_V4DF),
  LASX_BUILTIN (xvfmax_s, LARCH_V8SF_FTYPE_V8SF_V8SF),
  LASX_BUILTIN (xvfmax_d, LARCH_V4DF_FTYPE_V4DF_V4DF),
  LASX_BUILTIN (xvfmaxa_s, LARCH_V8SF_FTYPE_V8SF_V8SF),
  LASX_BUILTIN (xvfmaxa_d, LARCH_V4DF_FTYPE_V4DF_V4DF),
  LASX_BUILTIN (xvfclass_s, LARCH_V8SI_FTYPE_V8SF),
  LASX_BUILTIN (xvfclass_d, LARCH_V4DI_FTYPE_V4DF),
  LASX_BUILTIN (xvfsqrt_s, LARCH_V8SF_FTYPE_V8SF),
  LASX_BUILTIN (xvfsqrt_d, LARCH_V4DF_FTYPE_V4DF),
  LASX_BUILTIN (xvfrecip_s, LARCH_V8SF_FTYPE_V8SF),
  LASX_BUILTIN (xvfrecip_d, LARCH_V4DF_FTYPE_V4DF),
  LASX_BUILTIN (xvfrint_s, LARCH_V8SF_FTYPE_V8SF),
  LASX_BUILTIN (xvfrint_d, LARCH_V4DF_FTYPE_V4DF),
  LASX_BUILTIN (xvfrsqrt_s, LARCH_V8SF_FTYPE_V8SF),
  LASX_BUILTIN (xvfrsqrt_d, LARCH_V4DF_FTYPE_V4DF),
  LASX_BUILTIN (xvflogb_s, LARCH_V8SF_FTYPE_V8SF),
  LASX_BUILTIN (xvflogb_d, LARCH_V4DF_FTYPE_V4DF),
  LASX_BUILTIN (xvfcvth_s_h, LARCH_V8SF_FTYPE_V16HI),
  LASX_BUILTIN (xvfcvth_d_s, LARCH_V4DF_FTYPE_V8SF),
  LASX_BUILTIN (xvfcvtl_s_h, LARCH_V8SF_FTYPE_V16HI),
  LASX_BUILTIN (xvfcvtl_d_s, LARCH_V4DF_FTYPE_V8SF),
  LASX_BUILTIN (xvftint_w_s, LARCH_V8SI_FTYPE_V8SF),
  LASX_BUILTIN (xvftint_l_d, LARCH_V4DI_FTYPE_V4DF),
  LASX_BUILTIN (xvftint_wu_s, LARCH_UV8SI_FTYPE_V8SF),
  LASX_BUILTIN (xvftint_lu_d, LARCH_UV4DI_FTYPE_V4DF),
  LASX_BUILTIN (xvftintrz_w_s, LARCH_V8SI_FTYPE_V8SF),
  LASX_BUILTIN (xvftintrz_l_d, LARCH_V4DI_FTYPE_V4DF),
  LASX_BUILTIN (xvftintrz_wu_s, LARCH_UV8SI_FTYPE_V8SF),
  LASX_BUILTIN (xvftintrz_lu_d, LARCH_UV4DI_FTYPE_V4DF),
  LASX_BUILTIN (xvffint_s_w, LARCH_V8SF_FTYPE_V8SI),
  LASX_BUILTIN (xvffint_d_l, LARCH_V4DF_FTYPE_V4DI),
  LASX_BUILTIN (xvffint_s_wu, LARCH_V8SF_FTYPE_UV8SI),
  LASX_BUILTIN (xvffint_d_lu, LARCH_V4DF_FTYPE_UV4DI),

  LASX_BUILTIN (xvreplve_b, LARCH_V32QI_FTYPE_V32QI_SI),
  LASX_BUILTIN (xvreplve_h, LARCH_V16HI_FTYPE_V16HI_SI),
  LASX_BUILTIN (xvreplve_w, LARCH_V8SI_FTYPE_V8SI_SI),
  LASX_BUILTIN (xvreplve_d, LARCH_V4DI_FTYPE_V4DI_SI),
  LASX_BUILTIN (xvpermi_w, LARCH_V8SI_FTYPE_V8SI_V8SI_USI),

  LASX_BUILTIN (xvandn_v, LARCH_UV32QI_FTYPE_UV32QI_UV32QI),
  LASX_BUILTIN (xvneg_b, LARCH_V32QI_FTYPE_V32QI),
  LASX_BUILTIN (xvneg_h, LARCH_V16HI_FTYPE_V16HI),
  LASX_BUILTIN (xvneg_w, LARCH_V8SI_FTYPE_V8SI),
  LASX_BUILTIN (xvneg_d, LARCH_V4DI_FTYPE_V4DI),
  LASX_BUILTIN (xvmuh_b, LARCH_V32QI_FTYPE_V32QI_V32QI),
  LASX_BUILTIN (xvmuh_h, LARCH_V16HI_FTYPE_V16HI_V16HI),
  LASX_BUILTIN (xvmuh_w, LARCH_V8SI_FTYPE_V8SI_V8SI),
  LASX_BUILTIN (xvmuh_d, LARCH_V4DI_FTYPE_V4DI_V4DI),
  LASX_BUILTIN (xvmuh_bu, LARCH_UV32QI_FTYPE_UV32QI_UV32QI),
  LASX_BUILTIN (xvmuh_hu, LARCH_UV16HI_FTYPE_UV16HI_UV16HI),
  LASX_BUILTIN (xvmuh_wu, LARCH_UV8SI_FTYPE_UV8SI_UV8SI),
  LASX_BUILTIN (xvmuh_du, LARCH_UV4DI_FTYPE_UV4DI_UV4DI),
  LASX_BUILTIN (xvsllwil_h_b, LARCH_V16HI_FTYPE_V32QI_UQI),
  LASX_BUILTIN (xvsllwil_w_h, LARCH_V8SI_FTYPE_V16HI_UQI),
  LASX_BUILTIN (xvsllwil_d_w, LARCH_V4DI_FTYPE_V8SI_UQI),
  LASX_BUILTIN (xvsllwil_hu_bu, LARCH_UV16HI_FTYPE_UV32QI_UQI), /* FIXME: U? */
  LASX_BUILTIN (xvsllwil_wu_hu, LARCH_UV8SI_FTYPE_UV16HI_UQI),
  LASX_BUILTIN (xvsllwil_du_wu, LARCH_UV4DI_FTYPE_UV8SI_UQI),
  LASX_BUILTIN (xvsran_b_h, LARCH_V32QI_FTYPE_V16HI_V16HI),
  LASX_BUILTIN (xvsran_h_w, LARCH_V16HI_FTYPE_V8SI_V8SI),
  LASX_BUILTIN (xvsran_w_d, LARCH_V8SI_FTYPE_V4DI_V4DI),
  LASX_BUILTIN (xvssran_b_h, LARCH_V32QI_FTYPE_V16HI_V16HI),
  LASX_BUILTIN (xvssran_h_w, LARCH_V16HI_FTYPE_V8SI_V8SI),
  LASX_BUILTIN (xvssran_w_d, LARCH_V8SI_FTYPE_V4DI_V4DI),
  LASX_BUILTIN (xvssran_bu_h, LARCH_UV32QI_FTYPE_UV16HI_UV16HI),
  LASX_BUILTIN (xvssran_hu_w, LARCH_UV16HI_FTYPE_UV8SI_UV8SI),
  LASX_BUILTIN (xvssran_wu_d, LARCH_UV8SI_FTYPE_UV4DI_UV4DI),
  LASX_BUILTIN (xvsrarn_b_h, LARCH_V32QI_FTYPE_V16HI_V16HI),
  LASX_BUILTIN (xvsrarn_h_w, LARCH_V16HI_FTYPE_V8SI_V8SI),
  LASX_BUILTIN (xvsrarn_w_d, LARCH_V8SI_FTYPE_V4DI_V4DI),
  LASX_BUILTIN (xvssrarn_b_h, LARCH_V32QI_FTYPE_V16HI_V16HI),
  LASX_BUILTIN (xvssrarn_h_w, LARCH_V16HI_FTYPE_V8SI_V8SI),
  LASX_BUILTIN (xvssrarn_w_d, LARCH_V8SI_FTYPE_V4DI_V4DI),
  LASX_BUILTIN (xvssrarn_bu_h, LARCH_UV32QI_FTYPE_UV16HI_UV16HI),
  LASX_BUILTIN (xvssrarn_hu_w, LARCH_UV16HI_FTYPE_UV8SI_UV8SI),
  LASX_BUILTIN (xvssrarn_wu_d, LARCH_UV8SI_FTYPE_UV4DI_UV4DI),
  LASX_BUILTIN (xvsrln_b_h, LARCH_V32QI_FTYPE_V16HI_V16HI),
  LASX_BUILTIN (xvsrln_h_w, LARCH_V16HI_FTYPE_V8SI_V8SI),
  LASX_BUILTIN (xvsrln_w_d, LARCH_V8SI_FTYPE_V4DI_V4DI),
  LASX_BUILTIN (xvssrln_bu_h, LARCH_UV32QI_FTYPE_UV16HI_UV16HI),
  LASX_BUILTIN (xvssrln_hu_w, LARCH_UV16HI_FTYPE_UV8SI_UV8SI),
  LASX_BUILTIN (xvssrln_wu_d, LARCH_UV8SI_FTYPE_UV4DI_UV4DI),
  LASX_BUILTIN (xvsrlrn_b_h, LARCH_V32QI_FTYPE_V16HI_V16HI),
  LASX_BUILTIN (xvsrlrn_h_w, LARCH_V16HI_FTYPE_V8SI_V8SI),
  LASX_BUILTIN (xvsrlrn_w_d, LARCH_V8SI_FTYPE_V4DI_V4DI),
  LASX_BUILTIN (xvssrlrn_bu_h, LARCH_UV32QI_FTYPE_UV16HI_UV16HI),
  LASX_BUILTIN (xvssrlrn_hu_w, LARCH_UV16HI_FTYPE_UV8SI_UV8SI),
  LASX_BUILTIN (xvssrlrn_wu_d, LARCH_UV8SI_FTYPE_UV4DI_UV4DI),
  LASX_BUILTIN (xvfrstpi_b, LARCH_V32QI_FTYPE_V32QI_V32QI_UQI),
  LASX_BUILTIN (xvfrstpi_h, LARCH_V16HI_FTYPE_V16HI_V16HI_UQI),
  LASX_BUILTIN (xvfrstp_b, LARCH_V32QI_FTYPE_V32QI_V32QI_V32QI),
  LASX_BUILTIN (xvfrstp_h, LARCH_V16HI_FTYPE_V16HI_V16HI_V16HI),
  LASX_BUILTIN (xvshuf4i_d, LARCH_V4DI_FTYPE_V4DI_V4DI_USI),
  LASX_BUILTIN (xvbsrl_v, LARCH_V32QI_FTYPE_V32QI_UQI),
  LASX_BUILTIN (xvbsll_v, LARCH_V32QI_FTYPE_V32QI_UQI),
  LASX_BUILTIN (xvextrins_b, LARCH_V32QI_FTYPE_V32QI_V32QI_USI),
  LASX_BUILTIN (xvextrins_h, LARCH_V16HI_FTYPE_V16HI_V16HI_USI),
  LASX_BUILTIN (xvextrins_w, LARCH_V8SI_FTYPE_V8SI_V8SI_USI),
  LASX_BUILTIN (xvextrins_d, LARCH_V4DI_FTYPE_V4DI_V4DI_USI),
  LASX_BUILTIN (xvmskltz_b, LARCH_V32QI_FTYPE_V32QI),
  LASX_BUILTIN (xvmskltz_h, LARCH_V16HI_FTYPE_V16HI),
  LASX_BUILTIN (xvmskltz_w, LARCH_V8SI_FTYPE_V8SI),
  LASX_BUILTIN (xvmskltz_d, LARCH_V4DI_FTYPE_V4DI),
  LASX_BUILTIN (xvsigncov_b, LARCH_V32QI_FTYPE_V32QI_V32QI),
  LASX_BUILTIN (xvsigncov_h, LARCH_V16HI_FTYPE_V16HI_V16HI),
  LASX_BUILTIN (xvsigncov_w, LARCH_V8SI_FTYPE_V8SI_V8SI),
  LASX_BUILTIN (xvsigncov_d, LARCH_V4DI_FTYPE_V4DI_V4DI),
  LASX_BUILTIN (xvfmadd_s, LARCH_V8SF_FTYPE_V8SF_V8SF_V8SF),
  LASX_BUILTIN (xvfmadd_d, LARCH_V4DF_FTYPE_V4DF_V4DF_V4DF),
  LASX_BUILTIN (xvfmsub_s, LARCH_V8SF_FTYPE_V8SF_V8SF_V8SF),
  LASX_BUILTIN (xvfmsub_d, LARCH_V4DF_FTYPE_V4DF_V4DF_V4DF),
  LASX_BUILTIN (xvfnmadd_s, LARCH_V8SF_FTYPE_V8SF_V8SF_V8SF),
  LASX_BUILTIN (xvfnmadd_d, LARCH_V4DF_FTYPE_V4DF_V4DF_V4DF),
  LASX_BUILTIN (xvfnmsub_s, LARCH_V8SF_FTYPE_V8SF_V8SF_V8SF),
  LASX_BUILTIN (xvfnmsub_d, LARCH_V4DF_FTYPE_V4DF_V4DF_V4DF),
  LASX_BUILTIN (xvftintrne_w_s, LARCH_V8SI_FTYPE_V8SF),
  LASX_BUILTIN (xvftintrne_l_d, LARCH_V4DI_FTYPE_V4DF),
  LASX_BUILTIN (xvftintrp_w_s, LARCH_V8SI_FTYPE_V8SF),
  LASX_BUILTIN (xvftintrp_l_d, LARCH_V4DI_FTYPE_V4DF),
  LASX_BUILTIN (xvftintrm_w_s, LARCH_V8SI_FTYPE_V8SF),
  LASX_BUILTIN (xvftintrm_l_d, LARCH_V4DI_FTYPE_V4DF),
  LASX_BUILTIN (xvftint_w_d, LARCH_V8SI_FTYPE_V4DF_V4DF),
  LASX_BUILTIN (xvffint_s_l, LARCH_V8SF_FTYPE_V4DI_V4DI),
  LASX_BUILTIN (xvftintrz_w_d, LARCH_V8SI_FTYPE_V4DF_V4DF),
  LASX_BUILTIN (xvftintrp_w_d, LARCH_V8SI_FTYPE_V4DF_V4DF),
  LASX_BUILTIN (xvftintrm_w_d, LARCH_V8SI_FTYPE_V4DF_V4DF),
  LASX_BUILTIN (xvftintrne_w_d, LARCH_V8SI_FTYPE_V4DF_V4DF),
  LASX_BUILTIN (xvftinth_l_s, LARCH_V4DI_FTYPE_V8SF),
  LASX_BUILTIN (xvftintl_l_s, LARCH_V4DI_FTYPE_V8SF),
  LASX_BUILTIN (xvffinth_d_w, LARCH_V4DF_FTYPE_V8SI),
  LASX_BUILTIN (xvffintl_d_w, LARCH_V4DF_FTYPE_V8SI),
  LASX_BUILTIN (xvftintrzh_l_s, LARCH_V4DI_FTYPE_V8SF),
  LASX_BUILTIN (xvftintrzl_l_s, LARCH_V4DI_FTYPE_V8SF),
  LASX_BUILTIN (xvftintrph_l_s, LARCH_V4DI_FTYPE_V8SF),
  LASX_BUILTIN (xvftintrpl_l_s, LARCH_V4DI_FTYPE_V8SF),
  LASX_BUILTIN (xvftintrmh_l_s, LARCH_V4DI_FTYPE_V8SF),
  LASX_BUILTIN (xvftintrml_l_s, LARCH_V4DI_FTYPE_V8SF),
  LASX_BUILTIN (xvftintrneh_l_s, LARCH_V4DI_FTYPE_V8SF),
  LASX_BUILTIN (xvftintrnel_l_s, LARCH_V4DI_FTYPE_V8SF),
  LASX_BUILTIN (xvfrintrne_s, LARCH_V8SF_FTYPE_V8SF),
  LASX_BUILTIN (xvfrintrne_d, LARCH_V4DF_FTYPE_V4DF),
  LASX_BUILTIN (xvfrintrz_s, LARCH_V8SF_FTYPE_V8SF),
  LASX_BUILTIN (xvfrintrz_d, LARCH_V4DF_FTYPE_V4DF),
  LASX_BUILTIN (xvfrintrp_s, LARCH_V8SF_FTYPE_V8SF),
  LASX_BUILTIN (xvfrintrp_d, LARCH_V4DF_FTYPE_V4DF),
  LASX_BUILTIN (xvfrintrm_s, LARCH_V8SF_FTYPE_V8SF),
  LASX_BUILTIN (xvfrintrm_d, LARCH_V4DF_FTYPE_V4DF),
  LASX_BUILTIN (xvld, LARCH_V32QI_FTYPE_CVPOINTER_SI),
  LASX_NO_TARGET_BUILTIN (xvst, LARCH_VOID_FTYPE_V32QI_CVPOINTER_SI),
  LASX_NO_TARGET_BUILTIN (xvstelm_b, LARCH_VOID_FTYPE_V32QI_CVPOINTER_SI_UQI),
  LASX_NO_TARGET_BUILTIN (xvstelm_h, LARCH_VOID_FTYPE_V16HI_CVPOINTER_SI_UQI),
  LASX_NO_TARGET_BUILTIN (xvstelm_w, LARCH_VOID_FTYPE_V8SI_CVPOINTER_SI_UQI),
  LASX_NO_TARGET_BUILTIN (xvstelm_d, LARCH_VOID_FTYPE_V4DI_CVPOINTER_SI_UQI),
  LASX_BUILTIN (xvinsve0_w, LARCH_V8SI_FTYPE_V8SI_V8SI_UQI),
  LASX_BUILTIN (xvinsve0_d, LARCH_V4DI_FTYPE_V4DI_V4DI_UQI),
  LASX_BUILTIN (xvpickve_w, LARCH_V8SI_FTYPE_V8SI_UQI),
  LASX_BUILTIN (xvpickve_d, LARCH_V4DI_FTYPE_V4DI_UQI),
  LASX_BUILTIN (xvpickve_w_f, LARCH_V8SF_FTYPE_V8SF_UQI),
  LASX_BUILTIN (xvpickve_d_f, LARCH_V4DF_FTYPE_V4DF_UQI),
  LASX_BUILTIN (xvssrlrn_b_h, LARCH_V32QI_FTYPE_V16HI_V16HI),
  LASX_BUILTIN (xvssrlrn_h_w, LARCH_V16HI_FTYPE_V8SI_V8SI),
  LASX_BUILTIN (xvssrlrn_w_d, LARCH_V8SI_FTYPE_V4DI_V4DI),
  LASX_BUILTIN (xvssrln_b_h, LARCH_V32QI_FTYPE_V16HI_V16HI),
  LASX_BUILTIN (xvssrln_h_w, LARCH_V16HI_FTYPE_V8SI_V8SI),
  LASX_BUILTIN (xvssrln_w_d, LARCH_V8SI_FTYPE_V4DI_V4DI),
  LASX_BUILTIN (xvorn_v, LARCH_UV32QI_FTYPE_UV32QI_UV32QI),
  LASX_BUILTIN (xvldi, LARCH_V4DI_FTYPE_HI),
  LASX_BUILTIN (xvldx, LARCH_V32QI_FTYPE_CVPOINTER_DI),
  LASX_NO_TARGET_BUILTIN (xvstx, LARCH_VOID_FTYPE_V32QI_CVPOINTER_DI),
  LASX_BUILTIN (xvextl_qu_du, LARCH_UV4DI_FTYPE_UV4DI),

  /* LASX */
  LASX_BUILTIN (xvinsgr2vr_w, LARCH_V8SI_FTYPE_V8SI_SI_UQI),
  LASX_BUILTIN (xvinsgr2vr_d, LARCH_V4DI_FTYPE_V4DI_DI_UQI),

  LASX_BUILTIN (xvreplve0_b, LARCH_V32QI_FTYPE_V32QI),
  LASX_BUILTIN (xvreplve0_h, LARCH_V16HI_FTYPE_V16HI),
  LASX_BUILTIN (xvreplve0_w, LARCH_V8SI_FTYPE_V8SI),
  LASX_BUILTIN (xvreplve0_d, LARCH_V4DI_FTYPE_V4DI),
  LASX_BUILTIN (xvreplve0_q, LARCH_V32QI_FTYPE_V32QI),
  LASX_BUILTIN (vext2xv_h_b, LARCH_V16HI_FTYPE_V32QI),
  LASX_BUILTIN (vext2xv_w_h, LARCH_V8SI_FTYPE_V16HI),
  LASX_BUILTIN (vext2xv_d_w, LARCH_V4DI_FTYPE_V8SI),
  LASX_BUILTIN (vext2xv_w_b, LARCH_V8SI_FTYPE_V32QI),
  LASX_BUILTIN (vext2xv_d_h, LARCH_V4DI_FTYPE_V16HI),
  LASX_BUILTIN (vext2xv_d_b, LARCH_V4DI_FTYPE_V32QI),
  LASX_BUILTIN (vext2xv_hu_bu, LARCH_V16HI_FTYPE_V32QI),
  LASX_BUILTIN (vext2xv_wu_hu, LARCH_V8SI_FTYPE_V16HI),
  LASX_BUILTIN (vext2xv_du_wu, LARCH_V4DI_FTYPE_V8SI),
  LASX_BUILTIN (vext2xv_wu_bu, LARCH_V8SI_FTYPE_V32QI),
  LASX_BUILTIN (vext2xv_du_hu, LARCH_V4DI_FTYPE_V16HI),
  LASX_BUILTIN (vext2xv_du_bu, LARCH_V4DI_FTYPE_V32QI),
  LASX_BUILTIN (xvpermi_q, LARCH_V32QI_FTYPE_V32QI_V32QI_USI),
  LASX_BUILTIN (xvpermi_d, LARCH_V4DI_FTYPE_V4DI_USI),
  LASX_BUILTIN (xvperm_w, LARCH_V8SI_FTYPE_V8SI_V8SI),
  LASX_BUILTIN_TEST_BRANCH (xbz_b, LARCH_SI_FTYPE_UV32QI),
  LASX_BUILTIN_TEST_BRANCH (xbz_h, LARCH_SI_FTYPE_UV16HI),
  LASX_BUILTIN_TEST_BRANCH (xbz_w, LARCH_SI_FTYPE_UV8SI),
  LASX_BUILTIN_TEST_BRANCH (xbz_d, LARCH_SI_FTYPE_UV4DI),
  LASX_BUILTIN_TEST_BRANCH (xbnz_b, LARCH_SI_FTYPE_UV32QI),
  LASX_BUILTIN_TEST_BRANCH (xbnz_h, LARCH_SI_FTYPE_UV16HI),
  LASX_BUILTIN_TEST_BRANCH (xbnz_w, LARCH_SI_FTYPE_UV8SI),
  LASX_BUILTIN_TEST_BRANCH (xbnz_d, LARCH_SI_FTYPE_UV4DI),
  LASX_BUILTIN_TEST_BRANCH (xbz_v, LARCH_SI_FTYPE_UV32QI),
  LASX_BUILTIN_TEST_BRANCH (xbnz_v, LARCH_SI_FTYPE_UV32QI),
  LASX_BUILTIN (xvldrepl_b, LARCH_V32QI_FTYPE_CVPOINTER_SI),
  LASX_BUILTIN (xvldrepl_h, LARCH_V16HI_FTYPE_CVPOINTER_SI),
  LASX_BUILTIN (xvldrepl_w, LARCH_V8SI_FTYPE_CVPOINTER_SI),
  LASX_BUILTIN (xvldrepl_d, LARCH_V4DI_FTYPE_CVPOINTER_SI),
  LASX_BUILTIN (xvpickve2gr_w, LARCH_SI_FTYPE_V8SI_UQI),
  LASX_BUILTIN (xvpickve2gr_wu, LARCH_USI_FTYPE_V8SI_UQI),
  LASX_BUILTIN (xvpickve2gr_d, LARCH_DI_FTYPE_V4DI_UQI),
  LASX_BUILTIN (xvpickve2gr_du, LARCH_UDI_FTYPE_V4DI_UQI),

  LASX_BUILTIN (xvaddwev_q_d, LARCH_V4DI_FTYPE_V4DI_V4DI),
  LASX_BUILTIN (xvaddwev_d_w, LARCH_V4DI_FTYPE_V8SI_V8SI),
  LASX_BUILTIN (xvaddwev_w_h, LARCH_V8SI_FTYPE_V16HI_V16HI),
  LASX_BUILTIN (xvaddwev_h_b, LARCH_V16HI_FTYPE_V32QI_V32QI),
  LASX_BUILTIN (xvaddwev_q_du, LARCH_V4DI_FTYPE_UV4DI_UV4DI),
  LASX_BUILTIN (xvaddwev_d_wu, LARCH_V4DI_FTYPE_UV8SI_UV8SI),
  LASX_BUILTIN (xvaddwev_w_hu, LARCH_V8SI_FTYPE_UV16HI_UV16HI),
  LASX_BUILTIN (xvaddwev_h_bu, LARCH_V16HI_FTYPE_UV32QI_UV32QI),
  LASX_BUILTIN (xvsubwev_q_d, LARCH_V4DI_FTYPE_V4DI_V4DI),
  LASX_BUILTIN (xvsubwev_d_w, LARCH_V4DI_FTYPE_V8SI_V8SI),
  LASX_BUILTIN (xvsubwev_w_h, LARCH_V8SI_FTYPE_V16HI_V16HI),
  LASX_BUILTIN (xvsubwev_h_b, LARCH_V16HI_FTYPE_V32QI_V32QI),
  LASX_BUILTIN (xvsubwev_q_du, LARCH_V4DI_FTYPE_UV4DI_UV4DI),
  LASX_BUILTIN (xvsubwev_d_wu, LARCH_V4DI_FTYPE_UV8SI_UV8SI),
  LASX_BUILTIN (xvsubwev_w_hu, LARCH_V8SI_FTYPE_UV16HI_UV16HI),
  LASX_BUILTIN (xvsubwev_h_bu, LARCH_V16HI_FTYPE_UV32QI_UV32QI),
  LASX_BUILTIN (xvmulwev_q_d, LARCH_V4DI_FTYPE_V4DI_V4DI),
  LASX_BUILTIN (xvmulwev_d_w, LARCH_V4DI_FTYPE_V8SI_V8SI),
  LASX_BUILTIN (xvmulwev_w_h, LARCH_V8SI_FTYPE_V16HI_V16HI),
  LASX_BUILTIN (xvmulwev_h_b, LARCH_V16HI_FTYPE_V32QI_V32QI),
  LASX_BUILTIN (xvmulwev_q_du, LARCH_V4DI_FTYPE_UV4DI_UV4DI),
  LASX_BUILTIN (xvmulwev_d_wu, LARCH_V4DI_FTYPE_UV8SI_UV8SI),
  LASX_BUILTIN (xvmulwev_w_hu, LARCH_V8SI_FTYPE_UV16HI_UV16HI),
  LASX_BUILTIN (xvmulwev_h_bu, LARCH_V16HI_FTYPE_UV32QI_UV32QI),
  LASX_BUILTIN (xvaddwod_q_d, LARCH_V4DI_FTYPE_V4DI_V4DI),
  LASX_BUILTIN (xvaddwod_d_w, LARCH_V4DI_FTYPE_V8SI_V8SI),
  LASX_BUILTIN (xvaddwod_w_h, LARCH_V8SI_FTYPE_V16HI_V16HI),
  LASX_BUILTIN (xvaddwod_h_b, LARCH_V16HI_FTYPE_V32QI_V32QI),
  LASX_BUILTIN (xvaddwod_q_du, LARCH_V4DI_FTYPE_UV4DI_UV4DI),
  LASX_BUILTIN (xvaddwod_d_wu, LARCH_V4DI_FTYPE_UV8SI_UV8SI),
  LASX_BUILTIN (xvaddwod_w_hu, LARCH_V8SI_FTYPE_UV16HI_UV16HI),
  LASX_BUILTIN (xvaddwod_h_bu, LARCH_V16HI_FTYPE_UV32QI_UV32QI),
  LASX_BUILTIN (xvsubwod_q_d, LARCH_V4DI_FTYPE_V4DI_V4DI),
  LASX_BUILTIN (xvsubwod_d_w, LARCH_V4DI_FTYPE_V8SI_V8SI),
  LASX_BUILTIN (xvsubwod_w_h, LARCH_V8SI_FTYPE_V16HI_V16HI),
  LASX_BUILTIN (xvsubwod_h_b, LARCH_V16HI_FTYPE_V32QI_V32QI),
  LASX_BUILTIN (xvsubwod_q_du, LARCH_V4DI_FTYPE_UV4DI_UV4DI),
  LASX_BUILTIN (xvsubwod_d_wu, LARCH_V4DI_FTYPE_UV8SI_UV8SI),
  LASX_BUILTIN (xvsubwod_w_hu, LARCH_V8SI_FTYPE_UV16HI_UV16HI),
  LASX_BUILTIN (xvsubwod_h_bu, LARCH_V16HI_FTYPE_UV32QI_UV32QI),
  LASX_BUILTIN (xvmulwod_q_d, LARCH_V4DI_FTYPE_V4DI_V4DI),
  LASX_BUILTIN (xvmulwod_d_w, LARCH_V4DI_FTYPE_V8SI_V8SI),
  LASX_BUILTIN (xvmulwod_w_h, LARCH_V8SI_FTYPE_V16HI_V16HI),
  LASX_BUILTIN (xvmulwod_h_b, LARCH_V16HI_FTYPE_V32QI_V32QI),
  LASX_BUILTIN (xvmulwod_q_du, LARCH_V4DI_FTYPE_UV4DI_UV4DI),
  LASX_BUILTIN (xvmulwod_d_wu, LARCH_V4DI_FTYPE_UV8SI_UV8SI),
  LASX_BUILTIN (xvmulwod_w_hu, LARCH_V8SI_FTYPE_UV16HI_UV16HI),
  LASX_BUILTIN (xvmulwod_h_bu, LARCH_V16HI_FTYPE_UV32QI_UV32QI),
  LASX_BUILTIN (xvaddwev_d_wu_w, LARCH_V4DI_FTYPE_UV8SI_V8SI),
  LASX_BUILTIN (xvaddwev_w_hu_h, LARCH_V8SI_FTYPE_UV16HI_V16HI),
  LASX_BUILTIN (xvaddwev_h_bu_b, LARCH_V16HI_FTYPE_UV32QI_V32QI),
  LASX_BUILTIN (xvmulwev_d_wu_w, LARCH_V4DI_FTYPE_UV8SI_V8SI),
  LASX_BUILTIN (xvmulwev_w_hu_h, LARCH_V8SI_FTYPE_UV16HI_V16HI),
  LASX_BUILTIN (xvmulwev_h_bu_b, LARCH_V16HI_FTYPE_UV32QI_V32QI),
  LASX_BUILTIN (xvaddwod_d_wu_w, LARCH_V4DI_FTYPE_UV8SI_V8SI),
  LASX_BUILTIN (xvaddwod_w_hu_h, LARCH_V8SI_FTYPE_UV16HI_V16HI),
  LASX_BUILTIN (xvaddwod_h_bu_b, LARCH_V16HI_FTYPE_UV32QI_V32QI),
  LASX_BUILTIN (xvmulwod_d_wu_w, LARCH_V4DI_FTYPE_UV8SI_V8SI),
  LASX_BUILTIN (xvmulwod_w_hu_h, LARCH_V8SI_FTYPE_UV16HI_V16HI),
  LASX_BUILTIN (xvmulwod_h_bu_b, LARCH_V16HI_FTYPE_UV32QI_V32QI),
  LASX_BUILTIN (xvhaddw_q_d, LARCH_V4DI_FTYPE_V4DI_V4DI),
  LASX_BUILTIN (xvhaddw_qu_du, LARCH_UV4DI_FTYPE_UV4DI_UV4DI),
  LASX_BUILTIN (xvhsubw_q_d, LARCH_V4DI_FTYPE_V4DI_V4DI),
  LASX_BUILTIN (xvhsubw_qu_du, LARCH_UV4DI_FTYPE_UV4DI_UV4DI),
  LASX_BUILTIN (xvmaddwev_q_d, LARCH_V4DI_FTYPE_V4DI_V4DI_V4DI),
  LASX_BUILTIN (xvmaddwev_d_w, LARCH_V4DI_FTYPE_V4DI_V8SI_V8SI),
  LASX_BUILTIN (xvmaddwev_w_h, LARCH_V8SI_FTYPE_V8SI_V16HI_V16HI),
  LASX_BUILTIN (xvmaddwev_h_b, LARCH_V16HI_FTYPE_V16HI_V32QI_V32QI),
  LASX_BUILTIN (xvmaddwev_q_du, LARCH_UV4DI_FTYPE_UV4DI_UV4DI_UV4DI),
  LASX_BUILTIN (xvmaddwev_d_wu, LARCH_UV4DI_FTYPE_UV4DI_UV8SI_UV8SI),
  LASX_BUILTIN (xvmaddwev_w_hu, LARCH_UV8SI_FTYPE_UV8SI_UV16HI_UV16HI),
  LASX_BUILTIN (xvmaddwev_h_bu, LARCH_UV16HI_FTYPE_UV16HI_UV32QI_UV32QI),
  LASX_BUILTIN (xvmaddwod_q_d, LARCH_V4DI_FTYPE_V4DI_V4DI_V4DI),
  LASX_BUILTIN (xvmaddwod_d_w, LARCH_V4DI_FTYPE_V4DI_V8SI_V8SI),
  LASX_BUILTIN (xvmaddwod_w_h, LARCH_V8SI_FTYPE_V8SI_V16HI_V16HI),
  LASX_BUILTIN (xvmaddwod_h_b, LARCH_V16HI_FTYPE_V16HI_V32QI_V32QI),
  LASX_BUILTIN (xvmaddwod_q_du, LARCH_UV4DI_FTYPE_UV4DI_UV4DI_UV4DI),
  LASX_BUILTIN (xvmaddwod_d_wu, LARCH_UV4DI_FTYPE_UV4DI_UV8SI_UV8SI),
  LASX_BUILTIN (xvmaddwod_w_hu, LARCH_UV8SI_FTYPE_UV8SI_UV16HI_UV16HI),
  LASX_BUILTIN (xvmaddwod_h_bu, LARCH_UV16HI_FTYPE_UV16HI_UV32QI_UV32QI),
  LASX_BUILTIN (xvmaddwev_q_du_d, LARCH_V4DI_FTYPE_V4DI_UV4DI_V4DI),
  LASX_BUILTIN (xvmaddwev_d_wu_w, LARCH_V4DI_FTYPE_V4DI_UV8SI_V8SI),
  LASX_BUILTIN (xvmaddwev_w_hu_h, LARCH_V8SI_FTYPE_V8SI_UV16HI_V16HI),
  LASX_BUILTIN (xvmaddwev_h_bu_b, LARCH_V16HI_FTYPE_V16HI_UV32QI_V32QI),
  LASX_BUILTIN (xvmaddwod_q_du_d, LARCH_V4DI_FTYPE_V4DI_UV4DI_V4DI),
  LASX_BUILTIN (xvmaddwod_d_wu_w, LARCH_V4DI_FTYPE_V4DI_UV8SI_V8SI),
  LASX_BUILTIN (xvmaddwod_w_hu_h, LARCH_V8SI_FTYPE_V8SI_UV16HI_V16HI),
  LASX_BUILTIN (xvmaddwod_h_bu_b, LARCH_V16HI_FTYPE_V16HI_UV32QI_V32QI),
  LASX_BUILTIN (xvrotr_b, LARCH_V32QI_FTYPE_V32QI_V32QI),
  LASX_BUILTIN (xvrotr_h, LARCH_V16HI_FTYPE_V16HI_V16HI),
  LASX_BUILTIN (xvrotr_w, LARCH_V8SI_FTYPE_V8SI_V8SI),
  LASX_BUILTIN (xvrotr_d, LARCH_V4DI_FTYPE_V4DI_V4DI),
  LASX_BUILTIN (xvadd_q, LARCH_V4DI_FTYPE_V4DI_V4DI),
  LASX_BUILTIN (xvsub_q, LARCH_V4DI_FTYPE_V4DI_V4DI),
  LASX_BUILTIN (xvaddwev_q_du_d, LARCH_V4DI_FTYPE_UV4DI_V4DI),
  LASX_BUILTIN (xvaddwod_q_du_d, LARCH_V4DI_FTYPE_UV4DI_V4DI),
  LASX_BUILTIN (xvmulwev_q_du_d, LARCH_V4DI_FTYPE_UV4DI_V4DI),
  LASX_BUILTIN (xvmulwod_q_du_d, LARCH_V4DI_FTYPE_UV4DI_V4DI),
  LASX_BUILTIN (xvmskgez_b, LARCH_V32QI_FTYPE_V32QI),
  LASX_BUILTIN (xvmsknz_b, LARCH_V32QI_FTYPE_V32QI),
  LASX_BUILTIN (xvexth_h_b, LARCH_V16HI_FTYPE_V32QI),
  LASX_BUILTIN (xvexth_w_h, LARCH_V8SI_FTYPE_V16HI),
  LASX_BUILTIN (xvexth_d_w, LARCH_V4DI_FTYPE_V8SI),
  LASX_BUILTIN (xvexth_q_d, LARCH_V4DI_FTYPE_V4DI),
  LASX_BUILTIN (xvexth_hu_bu, LARCH_UV16HI_FTYPE_UV32QI),
  LASX_BUILTIN (xvexth_wu_hu, LARCH_UV8SI_FTYPE_UV16HI),
  LASX_BUILTIN (xvexth_du_wu, LARCH_UV4DI_FTYPE_UV8SI),
  LASX_BUILTIN (xvexth_qu_du, LARCH_UV4DI_FTYPE_UV4DI),
  LASX_BUILTIN (xvrotri_b, LARCH_V32QI_FTYPE_V32QI_UQI),
  LASX_BUILTIN (xvrotri_h, LARCH_V16HI_FTYPE_V16HI_UQI),
  LASX_BUILTIN (xvrotri_w, LARCH_V8SI_FTYPE_V8SI_UQI),
  LASX_BUILTIN (xvrotri_d, LARCH_V4DI_FTYPE_V4DI_UQI),
  LASX_BUILTIN (xvextl_q_d, LARCH_V4DI_FTYPE_V4DI),
  LASX_BUILTIN (xvsrlni_b_h, LARCH_V32QI_FTYPE_V32QI_V32QI_USI),
  LASX_BUILTIN (xvsrlni_h_w, LARCH_V16HI_FTYPE_V16HI_V16HI_USI),
  LASX_BUILTIN (xvsrlni_w_d, LARCH_V8SI_FTYPE_V8SI_V8SI_USI),
  LASX_BUILTIN (xvsrlni_d_q, LARCH_V4DI_FTYPE_V4DI_V4DI_USI),
  LASX_BUILTIN (xvsrlrni_b_h, LARCH_V32QI_FTYPE_V32QI_V32QI_USI),
  LASX_BUILTIN (xvsrlrni_h_w, LARCH_V16HI_FTYPE_V16HI_V16HI_USI),
  LASX_BUILTIN (xvsrlrni_w_d, LARCH_V8SI_FTYPE_V8SI_V8SI_USI),
  LASX_BUILTIN (xvsrlrni_d_q, LARCH_V4DI_FTYPE_V4DI_V4DI_USI),
  LASX_BUILTIN (xvssrlni_b_h, LARCH_V32QI_FTYPE_V32QI_V32QI_USI),
  LASX_BUILTIN (xvssrlni_h_w, LARCH_V16HI_FTYPE_V16HI_V16HI_USI),
  LASX_BUILTIN (xvssrlni_w_d, LARCH_V8SI_FTYPE_V8SI_V8SI_USI),
  LASX_BUILTIN (xvssrlni_d_q, LARCH_V4DI_FTYPE_V4DI_V4DI_USI),
  LASX_BUILTIN (xvssrlni_bu_h, LARCH_UV32QI_FTYPE_UV32QI_V32QI_USI),
  LASX_BUILTIN (xvssrlni_hu_w, LARCH_UV16HI_FTYPE_UV16HI_V16HI_USI),
  LASX_BUILTIN (xvssrlni_wu_d, LARCH_UV8SI_FTYPE_UV8SI_V8SI_USI),
  LASX_BUILTIN (xvssrlni_du_q, LARCH_UV4DI_FTYPE_UV4DI_V4DI_USI),
  LASX_BUILTIN (xvssrlrni_b_h, LARCH_V32QI_FTYPE_V32QI_V32QI_USI),
  LASX_BUILTIN (xvssrlrni_h_w, LARCH_V16HI_FTYPE_V16HI_V16HI_USI),
  LASX_BUILTIN (xvssrlrni_w_d, LARCH_V8SI_FTYPE_V8SI_V8SI_USI),
  LASX_BUILTIN (xvssrlrni_d_q, LARCH_V4DI_FTYPE_V4DI_V4DI_USI),
  LASX_BUILTIN (xvssrlrni_bu_h, LARCH_UV32QI_FTYPE_UV32QI_V32QI_USI),
  LASX_BUILTIN (xvssrlrni_hu_w, LARCH_UV16HI_FTYPE_UV16HI_V16HI_USI),
  LASX_BUILTIN (xvssrlrni_wu_d, LARCH_UV8SI_FTYPE_UV8SI_V8SI_USI),
  LASX_BUILTIN (xvssrlrni_du_q, LARCH_UV4DI_FTYPE_UV4DI_V4DI_USI),
  LASX_BUILTIN (xvsrani_b_h, LARCH_V32QI_FTYPE_V32QI_V32QI_USI),
  LASX_BUILTIN (xvsrani_h_w, LARCH_V16HI_FTYPE_V16HI_V16HI_USI),
  LASX_BUILTIN (xvsrani_w_d, LARCH_V8SI_FTYPE_V8SI_V8SI_USI),
  LASX_BUILTIN (xvsrani_d_q, LARCH_V4DI_FTYPE_V4DI_V4DI_USI),
  LASX_BUILTIN (xvsrarni_b_h, LARCH_V32QI_FTYPE_V32QI_V32QI_USI),
  LASX_BUILTIN (xvsrarni_h_w, LARCH_V16HI_FTYPE_V16HI_V16HI_USI),
  LASX_BUILTIN (xvsrarni_w_d, LARCH_V8SI_FTYPE_V8SI_V8SI_USI),
  LASX_BUILTIN (xvsrarni_d_q, LARCH_V4DI_FTYPE_V4DI_V4DI_USI),
  LASX_BUILTIN (xvssrani_b_h, LARCH_V32QI_FTYPE_V32QI_V32QI_USI),
  LASX_BUILTIN (xvssrani_h_w, LARCH_V16HI_FTYPE_V16HI_V16HI_USI),
  LASX_BUILTIN (xvssrani_w_d, LARCH_V8SI_FTYPE_V8SI_V8SI_USI),
  LASX_BUILTIN (xvssrani_d_q, LARCH_V4DI_FTYPE_V4DI_V4DI_USI),
  LASX_BUILTIN (xvssrani_bu_h, LARCH_UV32QI_FTYPE_UV32QI_V32QI_USI),
  LASX_BUILTIN (xvssrani_hu_w, LARCH_UV16HI_FTYPE_UV16HI_V16HI_USI),
  LASX_BUILTIN (xvssrani_wu_d, LARCH_UV8SI_FTYPE_UV8SI_V8SI_USI),
  LASX_BUILTIN (xvssrani_du_q, LARCH_UV4DI_FTYPE_UV4DI_V4DI_USI),
  LASX_BUILTIN (xvssrarni_b_h, LARCH_V32QI_FTYPE_V32QI_V32QI_USI),
  LASX_BUILTIN (xvssrarni_h_w, LARCH_V16HI_FTYPE_V16HI_V16HI_USI),
  LASX_BUILTIN (xvssrarni_w_d, LARCH_V8SI_FTYPE_V8SI_V8SI_USI),
  LASX_BUILTIN (xvssrarni_d_q, LARCH_V4DI_FTYPE_V4DI_V4DI_USI),
  LASX_BUILTIN (xvssrarni_bu_h, LARCH_UV32QI_FTYPE_UV32QI_V32QI_USI),
  LASX_BUILTIN (xvssrarni_hu_w, LARCH_UV16HI_FTYPE_UV16HI_V16HI_USI),
  LASX_BUILTIN (xvssrarni_wu_d, LARCH_UV8SI_FTYPE_UV8SI_V8SI_USI),
  LASX_BUILTIN (xvssrarni_du_q, LARCH_UV4DI_FTYPE_UV4DI_V4DI_USI)
};

/* Index I is the function declaration for loongarch_builtins[I], or null if
   the function isn't defined on this target.  */
static GTY (()) tree loongarch_builtin_decls[ARRAY_SIZE (loongarch_builtins)];
/* Get the index I of the function declaration for loongarch_builtin_decls[I]
   using the instruction code or return null if not defined for the target.  */
static GTY (()) int loongarch_get_builtin_decl_index[NUM_INSN_CODES];


/* MODE is a vector mode whose elements have type TYPE.  Return the type
   of the vector itself.  */

static tree
loongarch_builtin_vector_type (tree type, machine_mode mode)
{
  static tree types[2 * (int) MAX_MACHINE_MODE];
  int mode_index;

  mode_index = (int) mode;

  if (TREE_CODE (type) == INTEGER_TYPE && TYPE_UNSIGNED (type))
    mode_index += MAX_MACHINE_MODE;

  if (types[mode_index] == NULL_TREE)
    types[mode_index] = build_vector_type_for_mode (type, mode);
  return types[mode_index];
}

/* Return a type for 'const volatile void *'.  */

static tree
loongarch_build_cvpointer_type (void)
{
  static tree cache;

  if (cache == NULL_TREE)
    cache = build_pointer_type (build_qualified_type (void_type_node,
						      TYPE_QUAL_CONST
						      | TYPE_QUAL_VOLATILE));
  return cache;
}

/* Source-level argument types.  */
#define LARCH_ATYPE_VOID void_type_node
#define LARCH_ATYPE_INT integer_type_node
#define LARCH_ATYPE_POINTER ptr_type_node
#define LARCH_ATYPE_CVPOINTER loongarch_build_cvpointer_type ()
#define LARCH_ATYPE_BOOLEAN boolean_type_node
/* Standard mode-based argument types.  */
#define LARCH_ATYPE_QI intQI_type_node
#define LARCH_ATYPE_UQI unsigned_intQI_type_node
#define LARCH_ATYPE_HI intHI_type_node
#define LARCH_ATYPE_UHI unsigned_intHI_type_node
#define LARCH_ATYPE_SI intSI_type_node
#define LARCH_ATYPE_USI unsigned_intSI_type_node
#define LARCH_ATYPE_DI intDI_type_node
#define LARCH_ATYPE_UDI unsigned_intDI_type_node
#define LARCH_ATYPE_SF float_type_node
#define LARCH_ATYPE_DF double_type_node

/* Vector argument types.  */
#define LARCH_ATYPE_V2SF						\
  loongarch_builtin_vector_type (float_type_node, V2SFmode)
#define LARCH_ATYPE_V2HI						\
  loongarch_builtin_vector_type (intHI_type_node, V2HImode)
#define LARCH_ATYPE_V2SI						\
  loongarch_builtin_vector_type (intSI_type_node, V2SImode)
#define LARCH_ATYPE_V4QI						\
  loongarch_builtin_vector_type (intQI_type_node, V4QImode)
#define LARCH_ATYPE_V4HI						\
  loongarch_builtin_vector_type (intHI_type_node, V4HImode)
#define LARCH_ATYPE_V8QI						\
  loongarch_builtin_vector_type (intQI_type_node, V8QImode)

#define LARCH_ATYPE_V2DI						\
  loongarch_builtin_vector_type (long_long_integer_type_node, V2DImode)
#define LARCH_ATYPE_V4SI						\
  loongarch_builtin_vector_type (intSI_type_node, V4SImode)
#define LARCH_ATYPE_V8HI						\
  loongarch_builtin_vector_type (intHI_type_node, V8HImode)
#define LARCH_ATYPE_V16QI						\
  loongarch_builtin_vector_type (intQI_type_node, V16QImode)
#define LARCH_ATYPE_V2DF						\
  loongarch_builtin_vector_type (double_type_node, V2DFmode)
#define LARCH_ATYPE_V4SF						\
  loongarch_builtin_vector_type (float_type_node, V4SFmode)

/* LoongArch ASX.  */
#define LARCH_ATYPE_V4DI						\
  loongarch_builtin_vector_type (long_long_integer_type_node, V4DImode)
#define LARCH_ATYPE_V8SI						\
  loongarch_builtin_vector_type (intSI_type_node, V8SImode)
#define LARCH_ATYPE_V16HI						\
  loongarch_builtin_vector_type (intHI_type_node, V16HImode)
#define LARCH_ATYPE_V32QI						\
  loongarch_builtin_vector_type (intQI_type_node, V32QImode)
#define LARCH_ATYPE_V4DF						\
  loongarch_builtin_vector_type (double_type_node, V4DFmode)
#define LARCH_ATYPE_V8SF						\
  loongarch_builtin_vector_type (float_type_node, V8SFmode)

#define LARCH_ATYPE_UV2DI					\
  loongarch_builtin_vector_type (long_long_unsigned_type_node, V2DImode)
#define LARCH_ATYPE_UV4SI					\
  loongarch_builtin_vector_type (unsigned_intSI_type_node, V4SImode)
#define LARCH_ATYPE_UV8HI					\
  loongarch_builtin_vector_type (unsigned_intHI_type_node, V8HImode)
#define LARCH_ATYPE_UV16QI					\
  loongarch_builtin_vector_type (unsigned_intQI_type_node, V16QImode)

#define LARCH_ATYPE_UV4DI					\
  loongarch_builtin_vector_type (long_long_unsigned_type_node, V4DImode)
#define LARCH_ATYPE_UV8SI					\
  loongarch_builtin_vector_type (unsigned_intSI_type_node, V8SImode)
#define LARCH_ATYPE_UV16HI					\
  loongarch_builtin_vector_type (unsigned_intHI_type_node, V16HImode)
#define LARCH_ATYPE_UV32QI					\
  loongarch_builtin_vector_type (unsigned_intQI_type_node, V32QImode)

#define LARCH_ATYPE_UV2SI					\
  loongarch_builtin_vector_type (unsigned_intSI_type_node, V2SImode)
#define LARCH_ATYPE_UV4HI					\
  loongarch_builtin_vector_type (unsigned_intHI_type_node, V4HImode)
#define LARCH_ATYPE_UV8QI					\
  loongarch_builtin_vector_type (unsigned_intQI_type_node, V8QImode)

/* LARCH_FTYPE_ATYPESN takes N LARCH_FTYPES-like type codes and lists
   their associated LARCH_ATYPEs.  */
#define LARCH_FTYPE_ATYPES1(A, B) LARCH_ATYPE_##A, LARCH_ATYPE_##B

#define LARCH_FTYPE_ATYPES2(A, B, C) \
  LARCH_ATYPE_##A, LARCH_ATYPE_##B, LARCH_ATYPE_##C

#define LARCH_FTYPE_ATYPES3(A, B, C, D) \
  LARCH_ATYPE_##A, LARCH_ATYPE_##B, LARCH_ATYPE_##C, LARCH_ATYPE_##D

#define LARCH_FTYPE_ATYPES4(A, B, C, D, E) \
  LARCH_ATYPE_##A, LARCH_ATYPE_##B, LARCH_ATYPE_##C, LARCH_ATYPE_##D, \
  LARCH_ATYPE_##E

/* Return the function type associated with function prototype TYPE.  */

static tree
loongarch_build_function_type (enum loongarch_function_type type)
{
  static tree types[(int) LARCH_MAX_FTYPE_MAX];

  if (types[(int) type] == NULL_TREE)
    switch (type)
      {
#define DEF_LARCH_FTYPE(NUM, ARGS) \
  case LARCH_FTYPE_NAME##NUM ARGS: \
    types[(int) type] \
      = build_function_type_list (LARCH_FTYPE_ATYPES##NUM ARGS, NULL_TREE); \
    break;
#include "config/loongarch/loongarch-ftypes.def"
#undef DEF_LARCH_FTYPE
      default:
	gcc_unreachable ();
      }

  return types[(int) type];
}

/* Implement TARGET_INIT_BUILTINS.  */

void
loongarch_init_builtins (void)
{
  const struct loongarch_builtin_description *d;
  unsigned int i;
  tree type;

  /* Register the type float128_type_node as a built-in type and
     give it an alias "__float128".  */
  (*lang_hooks.types.register_builtin_type) (float128_type_node,
					    "__float128");

  /* Iterate through all of the bdesc arrays, initializing all of the
     builtin functions.  */
  for (i = 0; i < ARRAY_SIZE (loongarch_builtins); i++)
    {
      d = &loongarch_builtins[i];
      type = loongarch_build_function_type (d->function_type);
      loongarch_builtin_decls[i]
	= add_builtin_function (d->name, type, i, BUILT_IN_MD, NULL,
			  NULL);
      loongarch_get_builtin_decl_index[d->icode] = i;
    }
}

/* Implement TARGET_BUILTIN_DECL.  */

tree
loongarch_builtin_decl (unsigned int code, bool initialize_p ATTRIBUTE_UNUSED)
{
  if (code >= ARRAY_SIZE (loongarch_builtins))
    return error_mark_node;
  return loongarch_builtin_decls[code];
}

/* Take argument ARGNO from EXP's argument list and convert it into
   an expand operand.  Store the operand in *OP.  */

static void
loongarch_prepare_builtin_arg (struct expand_operand *op, tree exp,
			       unsigned int argno)
{
  tree arg;
  rtx value;

  arg = CALL_EXPR_ARG (exp, argno);
  value = expand_normal (arg);
  create_input_operand (op, value, TYPE_MODE (TREE_TYPE (arg)));
}

/* Return a const_int vector of VAL with mode MODE.  */

rtx
loongarch_gen_const_int_vector (machine_mode mode, HOST_WIDE_INT val)
{
  rtx c = gen_int_mode (val, GET_MODE_INNER (mode));
  return gen_const_vec_duplicate (mode, c);
}

/* Expand instruction ICODE as part of a built-in function sequence.
   Use the first NOPS elements of OPS as the instruction's operands.
   HAS_TARGET_P is true if operand 0 is a target; it is false if the
   instruction has no target.

   Return the target rtx if HAS_TARGET_P, otherwise return const0_rtx.  */

static rtx
loongarch_expand_builtin_insn (enum insn_code icode, unsigned int nops,
			       struct expand_operand *ops, bool has_target_p)
{
  machine_mode imode;
  int rangelo = 0, rangehi = 0, error_opno = 0;

  switch (icode)
    {
    case CODE_FOR_lsx_vaddi_bu:
    case CODE_FOR_lsx_vaddi_hu:
    case CODE_FOR_lsx_vaddi_wu:
    case CODE_FOR_lsx_vaddi_du:
    case CODE_FOR_lsx_vslti_bu:
    case CODE_FOR_lsx_vslti_hu:
    case CODE_FOR_lsx_vslti_wu:
    case CODE_FOR_lsx_vslti_du:
    case CODE_FOR_lsx_vslei_bu:
    case CODE_FOR_lsx_vslei_hu:
    case CODE_FOR_lsx_vslei_wu:
    case CODE_FOR_lsx_vslei_du:
    case CODE_FOR_lsx_vmaxi_bu:
    case CODE_FOR_lsx_vmaxi_hu:
    case CODE_FOR_lsx_vmaxi_wu:
    case CODE_FOR_lsx_vmaxi_du:
    case CODE_FOR_lsx_vmini_bu:
    case CODE_FOR_lsx_vmini_hu:
    case CODE_FOR_lsx_vmini_wu:
    case CODE_FOR_lsx_vmini_du:
    case CODE_FOR_lsx_vsubi_bu:
    case CODE_FOR_lsx_vsubi_hu:
    case CODE_FOR_lsx_vsubi_wu:
    case CODE_FOR_lsx_vsubi_du:
    case CODE_FOR_lasx_xvaddi_bu:
    case CODE_FOR_lasx_xvaddi_hu:
    case CODE_FOR_lasx_xvaddi_wu:
    case CODE_FOR_lasx_xvaddi_du:
    case CODE_FOR_lasx_xvslti_bu:
    case CODE_FOR_lasx_xvslti_hu:
    case CODE_FOR_lasx_xvslti_wu:
    case CODE_FOR_lasx_xvslti_du:
    case CODE_FOR_lasx_xvslei_bu:
    case CODE_FOR_lasx_xvslei_hu:
    case CODE_FOR_lasx_xvslei_wu:
    case CODE_FOR_lasx_xvslei_du:
    case CODE_FOR_lasx_xvmaxi_bu:
    case CODE_FOR_lasx_xvmaxi_hu:
    case CODE_FOR_lasx_xvmaxi_wu:
    case CODE_FOR_lasx_xvmaxi_du:
    case CODE_FOR_lasx_xvmini_bu:
    case CODE_FOR_lasx_xvmini_hu:
    case CODE_FOR_lasx_xvmini_wu:
    case CODE_FOR_lasx_xvmini_du:
    case CODE_FOR_lasx_xvsubi_bu:
    case CODE_FOR_lasx_xvsubi_hu:
    case CODE_FOR_lasx_xvsubi_wu:
    case CODE_FOR_lasx_xvsubi_du:
      gcc_assert (has_target_p && nops == 3);
      /* We only generate a vector of constants iff the second argument
	 is an immediate.  We also validate the range of the immediate.  */
      if (CONST_INT_P (ops[2].value))
	{
	  rangelo = 0;
	  rangehi = 31;
	  if (IN_RANGE (INTVAL (ops[2].value), rangelo, rangehi))
	    {
	      ops[2].mode = ops[0].mode;
	      ops[2].value = loongarch_gen_const_int_vector (ops[2].mode,
							     INTVAL (ops[2].value));
	    }
	  else
	    error_opno = 2;
	}
      break;

    case CODE_FOR_lsx_vseqi_b:
    case CODE_FOR_lsx_vseqi_h:
    case CODE_FOR_lsx_vseqi_w:
    case CODE_FOR_lsx_vseqi_d:
    case CODE_FOR_lsx_vslti_b:
    case CODE_FOR_lsx_vslti_h:
    case CODE_FOR_lsx_vslti_w:
    case CODE_FOR_lsx_vslti_d:
    case CODE_FOR_lsx_vslei_b:
    case CODE_FOR_lsx_vslei_h:
    case CODE_FOR_lsx_vslei_w:
    case CODE_FOR_lsx_vslei_d:
    case CODE_FOR_lsx_vmaxi_b:
    case CODE_FOR_lsx_vmaxi_h:
    case CODE_FOR_lsx_vmaxi_w:
    case CODE_FOR_lsx_vmaxi_d:
    case CODE_FOR_lsx_vmini_b:
    case CODE_FOR_lsx_vmini_h:
    case CODE_FOR_lsx_vmini_w:
    case CODE_FOR_lsx_vmini_d:
    case CODE_FOR_lasx_xvseqi_b:
    case CODE_FOR_lasx_xvseqi_h:
    case CODE_FOR_lasx_xvseqi_w:
    case CODE_FOR_lasx_xvseqi_d:
    case CODE_FOR_lasx_xvslti_b:
    case CODE_FOR_lasx_xvslti_h:
    case CODE_FOR_lasx_xvslti_w:
    case CODE_FOR_lasx_xvslti_d:
    case CODE_FOR_lasx_xvslei_b:
    case CODE_FOR_lasx_xvslei_h:
    case CODE_FOR_lasx_xvslei_w:
    case CODE_FOR_lasx_xvslei_d:
    case CODE_FOR_lasx_xvmaxi_b:
    case CODE_FOR_lasx_xvmaxi_h:
    case CODE_FOR_lasx_xvmaxi_w:
    case CODE_FOR_lasx_xvmaxi_d:
    case CODE_FOR_lasx_xvmini_b:
    case CODE_FOR_lasx_xvmini_h:
    case CODE_FOR_lasx_xvmini_w:
    case CODE_FOR_lasx_xvmini_d:
      gcc_assert (has_target_p && nops == 3);
      /* We only generate a vector of constants iff the second argument
	 is an immediate.  We also validate the range of the immediate.  */
      if (CONST_INT_P (ops[2].value))
	{
	  rangelo = -16;
	  rangehi = 15;
	  if (IN_RANGE (INTVAL (ops[2].value), rangelo, rangehi))
	    {
	      ops[2].mode = ops[0].mode;
	      ops[2].value = loongarch_gen_const_int_vector (ops[2].mode,
							     INTVAL (ops[2].value));
	    }
	  else
	    error_opno = 2;
	}
      break;

    case CODE_FOR_lsx_vandi_b:
    case CODE_FOR_lsx_vori_b:
    case CODE_FOR_lsx_vnori_b:
    case CODE_FOR_lsx_vxori_b:
    case CODE_FOR_lasx_xvandi_b:
    case CODE_FOR_lasx_xvori_b:
    case CODE_FOR_lasx_xvnori_b:
    case CODE_FOR_lasx_xvxori_b:
      gcc_assert (has_target_p && nops == 3);
      if (!CONST_INT_P (ops[2].value))
	break;
      ops[2].mode = ops[0].mode;
      ops[2].value = loongarch_gen_const_int_vector (ops[2].mode,
						     INTVAL (ops[2].value));
      break;

    case CODE_FOR_lsx_vbitseli_b:
    case CODE_FOR_lasx_xvbitseli_b:
      gcc_assert (has_target_p && nops == 4);
      if (!CONST_INT_P (ops[3].value))
	break;
      ops[3].mode = ops[0].mode;
      ops[3].value = loongarch_gen_const_int_vector (ops[3].mode,
						     INTVAL (ops[3].value));
      break;

    case CODE_FOR_lsx_vreplgr2vr_b:
    case CODE_FOR_lsx_vreplgr2vr_h:
    case CODE_FOR_lsx_vreplgr2vr_w:
    case CODE_FOR_lsx_vreplgr2vr_d:
    case CODE_FOR_lasx_xvreplgr2vr_b:
    case CODE_FOR_lasx_xvreplgr2vr_h:
    case CODE_FOR_lasx_xvreplgr2vr_w:
    case CODE_FOR_lasx_xvreplgr2vr_d:
      /* Map the built-ins to vector fill operations.  We need fix up the mode
	 for the element being inserted.  */
      gcc_assert (has_target_p && nops == 2);
      imode = GET_MODE_INNER (ops[0].mode);
      ops[1].value = lowpart_subreg (imode, ops[1].value, ops[1].mode);
      ops[1].mode = imode;
      break;

    case CODE_FOR_lsx_vilvh_b:
    case CODE_FOR_lsx_vilvh_h:
    case CODE_FOR_lsx_vilvh_w:
    case CODE_FOR_lsx_vilvh_d:
    case CODE_FOR_lsx_vilvl_b:
    case CODE_FOR_lsx_vilvl_h:
    case CODE_FOR_lsx_vilvl_w:
    case CODE_FOR_lsx_vilvl_d:
    case CODE_FOR_lsx_vpackev_b:
    case CODE_FOR_lsx_vpackev_h:
    case CODE_FOR_lsx_vpackev_w:
    case CODE_FOR_lsx_vpackod_b:
    case CODE_FOR_lsx_vpackod_h:
    case CODE_FOR_lsx_vpackod_w:
    case CODE_FOR_lsx_vpickev_b:
    case CODE_FOR_lsx_vpickev_h:
    case CODE_FOR_lsx_vpickev_w:
    case CODE_FOR_lsx_vpickod_b:
    case CODE_FOR_lsx_vpickod_h:
    case CODE_FOR_lsx_vpickod_w:
    case CODE_FOR_lsx_vandn_v:
    case CODE_FOR_lsx_vftintrz_w_d:
    case CODE_FOR_lsx_vffint_s_l:
    case CODE_FOR_lasx_xvilvh_b:
    case CODE_FOR_lasx_xvilvh_h:
    case CODE_FOR_lasx_xvilvh_w:
    case CODE_FOR_lasx_xvilvh_d:
    case CODE_FOR_lasx_xvilvl_b:
    case CODE_FOR_lasx_xvilvl_h:
    case CODE_FOR_lasx_xvilvl_w:
    case CODE_FOR_lasx_xvilvl_d:
    case CODE_FOR_lasx_xvpackev_b:
    case CODE_FOR_lasx_xvpackev_h:
    case CODE_FOR_lasx_xvpackev_w:
    case CODE_FOR_lasx_xvpackod_b:
    case CODE_FOR_lasx_xvpackod_h:
    case CODE_FOR_lasx_xvpackod_w:
    case CODE_FOR_lasx_xvpickev_b:
    case CODE_FOR_lasx_xvpickev_h:
    case CODE_FOR_lasx_xvpickev_w:
    case CODE_FOR_lasx_xvpickod_b:
    case CODE_FOR_lasx_xvpickod_h:
    case CODE_FOR_lasx_xvpickod_w:
    case CODE_FOR_lasx_xvandn_v:
      /* Swap the operands 1 and 2 for interleave operations.  Built-ins follow
	 convention of ISA, which have op1 as higher component and op2 as lower
	 component.  However, the VEC_PERM op in tree and vec_concat in RTL
	 expects first operand to be lower component, because of which this
	 swap is needed for builtins.  */
      gcc_assert (has_target_p && nops == 3);
      std::swap (ops[1], ops[2]);
      break;

    case CODE_FOR_lsx_vslli_b:
    case CODE_FOR_lsx_vslli_h:
    case CODE_FOR_lsx_vslli_w:
    case CODE_FOR_lsx_vslli_d:
    case CODE_FOR_lsx_vsrai_b:
    case CODE_FOR_lsx_vsrai_h:
    case CODE_FOR_lsx_vsrai_w:
    case CODE_FOR_lsx_vsrai_d:
    case CODE_FOR_lsx_vsrli_b:
    case CODE_FOR_lsx_vsrli_h:
    case CODE_FOR_lsx_vsrli_w:
    case CODE_FOR_lsx_vsrli_d:
    case CODE_FOR_lasx_xvslli_b:
    case CODE_FOR_lasx_xvslli_h:
    case CODE_FOR_lasx_xvslli_w:
    case CODE_FOR_lasx_xvslli_d:
    case CODE_FOR_lasx_xvsrai_b:
    case CODE_FOR_lasx_xvsrai_h:
    case CODE_FOR_lasx_xvsrai_w:
    case CODE_FOR_lasx_xvsrai_d:
    case CODE_FOR_lasx_xvsrli_b:
    case CODE_FOR_lasx_xvsrli_h:
    case CODE_FOR_lasx_xvsrli_w:
    case CODE_FOR_lasx_xvsrli_d:
      gcc_assert (has_target_p && nops == 3);
      if (CONST_INT_P (ops[2].value))
	{
	  rangelo = 0;
	  rangehi = GET_MODE_UNIT_BITSIZE (ops[0].mode) - 1;
	  if (IN_RANGE (INTVAL (ops[2].value), rangelo, rangehi))
	    {
	      ops[2].mode = ops[0].mode;
	      ops[2].value = loongarch_gen_const_int_vector (ops[2].mode,
							     INTVAL (ops[2].value));
	    }
	  else
	    error_opno = 2;
	}
      break;

    case CODE_FOR_lsx_vinsgr2vr_b:
    case CODE_FOR_lsx_vinsgr2vr_h:
    case CODE_FOR_lsx_vinsgr2vr_w:
    case CODE_FOR_lsx_vinsgr2vr_d:
      /* Map the built-ins to insert operations.  We need to swap operands,
	 fix up the mode for the element being inserted, and generate
	 a bit mask for vec_merge.  */
      gcc_assert (has_target_p && nops == 4);
      std::swap (ops[1], ops[2]);
      imode = GET_MODE_INNER (ops[0].mode);
      ops[1].value = lowpart_subreg (imode, ops[1].value, ops[1].mode);
      ops[1].mode = imode;
      rangelo = 0;
      rangehi = GET_MODE_NUNITS (ops[0].mode) - 1;
      if (CONST_INT_P (ops[3].value)
	  && IN_RANGE (INTVAL (ops[3].value), rangelo, rangehi))
	ops[3].value = GEN_INT (1 << INTVAL (ops[3].value));
      else
	error_opno = 2;
      break;

      /* Map the built-ins to element insert operations.  We need to swap
	 operands and generate a bit mask.  */
      gcc_assert (has_target_p && nops == 4);
      std::swap (ops[1], ops[2]);
      std::swap (ops[1], ops[3]);
      rangelo = 0;
      rangehi = GET_MODE_NUNITS (ops[0].mode) - 1;
      if (CONST_INT_P (ops[3].value)
	  && IN_RANGE (INTVAL (ops[3].value), rangelo, rangehi))
	ops[3].value = GEN_INT (1 << INTVAL (ops[3].value));
      else
	error_opno = 2;
      break;

    case CODE_FOR_lsx_vshuf4i_b:
    case CODE_FOR_lsx_vshuf4i_h:
    case CODE_FOR_lsx_vshuf4i_w:
    case CODE_FOR_lsx_vshuf4i_w_f:
      gcc_assert (has_target_p && nops == 3);
      ops[2].value = loongarch_gen_const_int_vector_shuffle (ops[0].mode,
							     INTVAL (ops[2].value));
      break;

    case CODE_FOR_lasx_xvinsgr2vr_w:
    case CODE_FOR_lasx_xvinsgr2vr_d:
      /* Map the built-ins to insert operations.  We need to swap operands,
	 fix up the mode for the element being inserted, and generate
	 a bit mask for vec_merge.  */
      gcc_assert (has_target_p && nops == 4);
      std::swap (ops[1], ops[2]);
      imode = GET_MODE_INNER (ops[0].mode);
      ops[1].value = lowpart_subreg (imode, ops[1].value, ops[1].mode);
      ops[1].mode = imode;
      rangelo = 0;
      rangehi = GET_MODE_NUNITS (ops[0].mode) - 1;
      if (CONST_INT_P (ops[3].value)
	  && IN_RANGE (INTVAL (ops[3].value), rangelo, rangehi))
	ops[3].value = GEN_INT (1 << INTVAL (ops[3].value));
      else
	error_opno = 2;
      break;

    default:
      break;
  }

  if (error_opno != 0)
    {
      error ("argument %d to the built-in must be a constant"
	     " in range %d to %d", error_opno, rangelo, rangehi);
      return has_target_p ? gen_reg_rtx (ops[0].mode) : const0_rtx;
    }
  else if (!maybe_expand_insn (icode, nops, ops))
    {
      error ("invalid argument to built-in function");
      return has_target_p ? gen_reg_rtx (ops[0].mode) : const0_rtx;
    }
  return has_target_p ? ops[0].value : const0_rtx;
}

/* Expand a LARCH_BUILTIN_DIRECT or LARCH_BUILTIN_DIRECT_NO_TARGET function;
   HAS_TARGET_P says which.  EXP is the CALL_EXPR that calls the function
   and ICODE is the code of the associated .md pattern.  TARGET, if nonnull,
   suggests a good place to put the result.  */

static rtx
loongarch_expand_builtin_direct (enum insn_code icode, rtx target, tree exp,
				 bool has_target_p)
{
  struct expand_operand ops[MAX_RECOG_OPERANDS];
  int opno, argno;

  /* Map any target to operand 0.  */
  opno = 0;
  if (has_target_p)
    create_output_operand (&ops[opno++], target, TYPE_MODE (TREE_TYPE (exp)));

  /* For the vector reciprocal instructions, we need to construct a temporary
     parameter const1_vector.  */
  switch (icode)
    {
    case CODE_FOR_recipv8sf3:
    case CODE_FOR_recipv4df3:
    case CODE_FOR_recipv4sf3:
    case CODE_FOR_recipv2df3:
      loongarch_prepare_builtin_arg (&ops[2], exp, 0);
      create_input_operand (&ops[1], CONST1_RTX (ops[0].mode), ops[0].mode);
      return loongarch_expand_builtin_insn (icode, 3, ops, has_target_p);

    default:
      break;
    }

  /* Map the arguments to the other operands.  */
  gcc_assert (opno + call_expr_nargs (exp)
	      == insn_data[icode].n_generator_args);
  for (argno = 0; argno < call_expr_nargs (exp); argno++)
    loongarch_prepare_builtin_arg (&ops[opno++], exp, argno);

  return loongarch_expand_builtin_insn (icode, opno, ops, has_target_p);
}

/* Expand an LSX built-in for a compare and branch instruction specified by
   ICODE, set a general-purpose register to 1 if the branch was taken,
   0 otherwise.  */

static rtx
loongarch_expand_builtin_lsx_test_branch (enum insn_code icode, tree exp)
{
  struct expand_operand ops[3];
  rtx_insn *cbranch;
  rtx_code_label *true_label, *done_label;
  rtx cmp_result;

  true_label = gen_label_rtx ();
  done_label = gen_label_rtx ();

  create_input_operand (&ops[0], true_label, TYPE_MODE (TREE_TYPE (exp)));
  loongarch_prepare_builtin_arg (&ops[1], exp, 0);
  create_fixed_operand (&ops[2], const0_rtx);

  /* Make sure that the operand 1 is a REG.  */
  if (GET_CODE (ops[1].value) != REG)
    ops[1].value = force_reg (ops[1].mode, ops[1].value);

  if ((cbranch = maybe_gen_insn (icode, 3, ops)) == NULL_RTX)
    {
      error ("failed to expand built-in function");
      return const0_rtx;
    }

  cmp_result = gen_reg_rtx (SImode);

  /* First assume that CMP_RESULT is false.  */
  loongarch_emit_move (cmp_result, const0_rtx);

  /* Branch to TRUE_LABEL if CBRANCH is taken and DONE_LABEL otherwise.  */
  emit_jump_insn (cbranch);
  emit_jump_insn (gen_jump (done_label));
  emit_barrier ();

  /* Set CMP_RESULT to true if the branch was taken.  */
  emit_label (true_label);
  loongarch_emit_move (cmp_result, const1_rtx);

  emit_label (done_label);
  return cmp_result;
}

/* Implement TARGET_EXPAND_BUILTIN.  */

rtx
loongarch_expand_builtin (tree exp, rtx target, rtx subtarget ATTRIBUTE_UNUSED,
			  machine_mode mode ATTRIBUTE_UNUSED,
			  int ignore ATTRIBUTE_UNUSED)
{
  tree fndecl;
  unsigned int fcode;
  const struct loongarch_builtin_description *d;

  fndecl = TREE_OPERAND (CALL_EXPR_FN (exp), 0);
  fcode = DECL_MD_FUNCTION_CODE (fndecl);
  gcc_assert (fcode < ARRAY_SIZE (loongarch_builtins));
  d = &loongarch_builtins[fcode];

  if (!d->avail ())
    {
      error_at (EXPR_LOCATION (exp),
		"built-in function %qD is not enabled", fndecl);
      return target ? target : const0_rtx;
    }

  switch (d->builtin_type)
    {
    case LARCH_BUILTIN_DIRECT:
    case LARCH_BUILTIN_LSX:
    case LARCH_BUILTIN_LASX:
      return loongarch_expand_builtin_direct (d->icode, target, exp, true);

    case LARCH_BUILTIN_DIRECT_NO_TARGET:
      return loongarch_expand_builtin_direct (d->icode, target, exp, false);

    case LARCH_BUILTIN_LSX_TEST_BRANCH:
    case LARCH_BUILTIN_LASX_TEST_BRANCH:
      return loongarch_expand_builtin_lsx_test_branch (d->icode, exp);
    }
  gcc_unreachable ();
}

/* Implement TARGET_ATOMIC_ASSIGN_EXPAND_FENV.  */

void
loongarch_atomic_assign_expand_fenv (tree *hold, tree *clear, tree *update)
{
  if (!TARGET_HARD_FLOAT_ABI)
    return;
  tree exceptions_var = create_tmp_var_raw (LARCH_ATYPE_USI);
  tree fcsr_orig_var = create_tmp_var_raw (LARCH_ATYPE_USI);
  tree fcsr_mod_var = create_tmp_var_raw (LARCH_ATYPE_USI);
  tree const0 = build_int_cst (LARCH_ATYPE_UQI, 0);
  tree get_fcsr = loongarch_builtin_decls[LARCH_MOVFCSR2GR];
  tree set_fcsr = loongarch_builtin_decls[LARCH_MOVGR2FCSR];
  tree get_fcsr_hold_call = build_call_expr (get_fcsr, 1, const0);
  tree hold_assign_orig = build4 (TARGET_EXPR, LARCH_ATYPE_USI,
				  fcsr_orig_var, get_fcsr_hold_call,
				  NULL, NULL);
  tree hold_mod_val = build2 (BIT_AND_EXPR, LARCH_ATYPE_USI, fcsr_orig_var,
			      build_int_cst (LARCH_ATYPE_USI, 0xffe0ffe0));
  tree hold_assign_mod = build4 (TARGET_EXPR, LARCH_ATYPE_USI,
				 fcsr_mod_var, hold_mod_val, NULL, NULL);
  tree set_fcsr_hold_call = build_call_expr (set_fcsr, 2, const0,
					     fcsr_mod_var);
  tree hold_all = build2 (COMPOUND_EXPR, LARCH_ATYPE_USI, hold_assign_orig,
			  hold_assign_mod);
  *hold = build2 (COMPOUND_EXPR, void_type_node, hold_all, set_fcsr_hold_call);

  *clear = build_call_expr (set_fcsr, 2, const0, fcsr_mod_var);

  tree get_fcsr_update_call = build_call_expr (get_fcsr, 1, const0);
  *update = build4 (TARGET_EXPR, LARCH_ATYPE_USI, exceptions_var,
		    get_fcsr_update_call, NULL, NULL);
  tree set_fcsr_update_call = build_call_expr (set_fcsr, 2, const0,
					       fcsr_orig_var);
  *update = build2 (COMPOUND_EXPR, void_type_node, *update,
		    set_fcsr_update_call);
  tree atomic_feraiseexcept
    = builtin_decl_implicit (BUILT_IN_ATOMIC_FERAISEEXCEPT);
  tree int_exceptions_var = fold_convert (integer_type_node, exceptions_var);
  tree atomic_feraiseexcept_call = build_call_expr (atomic_feraiseexcept, 1,
						    int_exceptions_var);
  *update = build2 (COMPOUND_EXPR, void_type_node, *update,
		    atomic_feraiseexcept_call);
}

/* Implement TARGET_BUILTIN_VA_LIST.  */

tree
loongarch_build_builtin_va_list (void)
{
  return ptr_type_node;
}

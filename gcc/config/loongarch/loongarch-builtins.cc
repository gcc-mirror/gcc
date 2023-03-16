/* Subroutines used for expanding LoongArch builtins.
   Copyright (C) 2021-2023 Free Software Foundation, Inc.
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
#define CODE_FOR_lsx_vffint_s_w CODE_FOR_floatv4siv4sf2
#define CODE_FOR_lsx_vffint_d_l CODE_FOR_floatv2div2df2
#define CODE_FOR_lsx_vffint_s_wu CODE_FOR_floatunsv4siv4sf2
#define CODE_FOR_lsx_vffint_d_lu CODE_FOR_floatunsv2div2df2
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
#define CODE_FOR_lsx_vabsd_b CODE_FOR_lsx_vabsd_s_b
#define CODE_FOR_lsx_vabsd_h CODE_FOR_lsx_vabsd_s_h
#define CODE_FOR_lsx_vabsd_w CODE_FOR_lsx_vabsd_s_w
#define CODE_FOR_lsx_vabsd_d CODE_FOR_lsx_vabsd_s_d
#define CODE_FOR_lsx_vabsd_bu CODE_FOR_lsx_vabsd_u_bu
#define CODE_FOR_lsx_vabsd_hu CODE_FOR_lsx_vabsd_u_hu
#define CODE_FOR_lsx_vabsd_wu CODE_FOR_lsx_vabsd_u_wu
#define CODE_FOR_lsx_vabsd_du CODE_FOR_lsx_vabsd_u_du
#define CODE_FOR_lsx_vftint_w_s CODE_FOR_lsx_vftint_s_w_s
#define CODE_FOR_lsx_vftint_l_d CODE_FOR_lsx_vftint_s_l_d
#define CODE_FOR_lsx_vftint_wu_s CODE_FOR_lsx_vftint_u_wu_s
#define CODE_FOR_lsx_vftint_lu_d CODE_FOR_lsx_vftint_u_lu_d
#define CODE_FOR_lsx_vandn_v CODE_FOR_vandnv16qi3
#define CODE_FOR_lsx_vorn_v CODE_FOR_vornv16qi3
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

#define CODE_FOR_lsx_vmuh_b CODE_FOR_lsx_vmuh_s_b
#define CODE_FOR_lsx_vmuh_h CODE_FOR_lsx_vmuh_s_h
#define CODE_FOR_lsx_vmuh_w CODE_FOR_lsx_vmuh_s_w
#define CODE_FOR_lsx_vmuh_d CODE_FOR_lsx_vmuh_s_d
#define CODE_FOR_lsx_vmuh_bu CODE_FOR_lsx_vmuh_u_bu
#define CODE_FOR_lsx_vmuh_hu CODE_FOR_lsx_vmuh_u_hu
#define CODE_FOR_lsx_vmuh_wu CODE_FOR_lsx_vmuh_u_wu
#define CODE_FOR_lsx_vmuh_du CODE_FOR_lsx_vmuh_u_du
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
  LSX_BUILTIN (vorn_v, LARCH_V16QI_FTYPE_V16QI_V16QI),
  LSX_BUILTIN (vldi, LARCH_V2DI_FTYPE_HI),
  LSX_BUILTIN (vshuf_b, LARCH_V16QI_FTYPE_V16QI_V16QI_V16QI),
  LSX_BUILTIN (vldx, LARCH_V16QI_FTYPE_CVPOINTER_DI),
  LSX_NO_TARGET_BUILTIN (vstx, LARCH_VOID_FTYPE_V16QI_CVPOINTER_DI),
  LSX_BUILTIN (vextl_qu_du, LARCH_UV2DI_FTYPE_UV2DI)
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
      if (d->avail ())
	{
	  type = loongarch_build_function_type (d->function_type);
	  loongarch_builtin_decls[i]
	    = add_builtin_function (d->name, type, i, BUILT_IN_MD, NULL,
				    NULL);
	  loongarch_get_builtin_decl_index[d->icode] = i;
	}
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

/* Implement TARGET_VECTORIZE_BUILTIN_VECTORIZED_FUNCTION.  */

tree
loongarch_builtin_vectorized_function (unsigned int fn, tree type_out,
				       tree type_in)
{
  machine_mode in_mode, out_mode;
  int in_n, out_n;

  if (TREE_CODE (type_out) != VECTOR_TYPE
      || TREE_CODE (type_in) != VECTOR_TYPE
      || !ISA_HAS_LSX)
    return NULL_TREE;

  out_mode = TYPE_MODE (TREE_TYPE (type_out));
  out_n = TYPE_VECTOR_SUBPARTS (type_out);
  in_mode = TYPE_MODE (TREE_TYPE (type_in));
  in_n = TYPE_VECTOR_SUBPARTS (type_in);

  /* INSN is the name of the associated instruction pattern, without
     the leading CODE_FOR_.  */
#define LARCH_GET_BUILTIN(INSN) \
  loongarch_builtin_decls[loongarch_get_builtin_decl_index[CODE_FOR_##INSN]]

  switch (fn)
    {
    CASE_CFN_CEIL:
      if (out_mode == DFmode && in_mode == DFmode)
    {
      if (out_n == 2 && in_n == 2)
	return LARCH_GET_BUILTIN (lsx_vfrintrp_d);
    }
      if (out_mode == SFmode && in_mode == SFmode)
    {
      if (out_n == 4 && in_n == 4)
	return LARCH_GET_BUILTIN (lsx_vfrintrp_s);
    }
      break;

    CASE_CFN_TRUNC:
      if (out_mode == DFmode && in_mode == DFmode)
    {
      if (out_n == 2 && in_n == 2)
	return LARCH_GET_BUILTIN (lsx_vfrintrz_d);
    }
      if (out_mode == SFmode && in_mode == SFmode)
    {
      if (out_n == 4 && in_n == 4)
	return LARCH_GET_BUILTIN (lsx_vfrintrz_s);
    }
      break;

    CASE_CFN_RINT:
    CASE_CFN_ROUND:
      if (out_mode == DFmode && in_mode == DFmode)
    {
      if (out_n == 2 && in_n == 2)
	return LARCH_GET_BUILTIN (lsx_vfrint_d);
    }
      if (out_mode == SFmode && in_mode == SFmode)
    {
      if (out_n == 4 && in_n == 4)
	return LARCH_GET_BUILTIN (lsx_vfrint_s);
    }
      break;

    CASE_CFN_FLOOR:
      if (out_mode == DFmode && in_mode == DFmode)
    {
      if (out_n == 2 && in_n == 2)
	return LARCH_GET_BUILTIN (lsx_vfrintrm_d);
    }
      if (out_mode == SFmode && in_mode == SFmode)
    {
      if (out_n == 4 && in_n == 4)
	return LARCH_GET_BUILTIN (lsx_vfrintrm_s);
    }
      break;

    default:
      break;
    }

  return NULL_TREE;
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
      gcc_assert (has_target_p && nops == 3);
      if (!CONST_INT_P (ops[2].value))
	break;
      ops[2].mode = ops[0].mode;
      ops[2].value = loongarch_gen_const_int_vector (ops[2].mode,
						     INTVAL (ops[2].value));
      break;

    case CODE_FOR_lsx_vbitseli_b:
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
    error ("failed to expand built-in function");

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
  unsigned int fcode, avail;
  const struct loongarch_builtin_description *d;

  fndecl = TREE_OPERAND (CALL_EXPR_FN (exp), 0);
  fcode = DECL_MD_FUNCTION_CODE (fndecl);
  gcc_assert (fcode < ARRAY_SIZE (loongarch_builtins));
  d = &loongarch_builtins[fcode];
  avail = d->avail ();
  gcc_assert (avail != 0);
  switch (d->builtin_type)
    {
    case LARCH_BUILTIN_DIRECT:
    case LARCH_BUILTIN_LSX:
      return loongarch_expand_builtin_direct (d->icode, target, exp, true);

    case LARCH_BUILTIN_DIRECT_NO_TARGET:
      return loongarch_expand_builtin_direct (d->icode, target, exp, false);

    case LARCH_BUILTIN_LSX_TEST_BRANCH:
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

/* RISC-V-specific code for C family languages.
   Copyright (C) 2011-2023 Free Software Foundation, Inc.
   Contributed by Andrew Waterman (andrew@sifive.com).

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

#define INCLUDE_STRING
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "c-family/c-common.h"
#include "cpplib.h"
#include "c-family/c-pragma.h"
#include "target.h"
#include "tm_p.h"
#include "riscv-subset.h"

#define builtin_define(TXT) cpp_define (pfile, TXT)

static int
riscv_ext_version_value (unsigned major, unsigned minor)
{
  return (major * 1000000) + (minor * 1000);
}

/* Implement TARGET_CPU_CPP_BUILTINS.  */

void
riscv_cpu_cpp_builtins (cpp_reader *pfile)
{
  builtin_define ("__riscv");

  if (TARGET_RVC)
    builtin_define ("__riscv_compressed");

  if (TARGET_RVE)
    builtin_define ("__riscv_32e");

  if (TARGET_ATOMIC)
    builtin_define ("__riscv_atomic");

  if (TARGET_MUL)
    builtin_define ("__riscv_mul");
  if (TARGET_DIV)
    builtin_define ("__riscv_div");
  if (TARGET_DIV && TARGET_MUL)
    builtin_define ("__riscv_muldiv");

  builtin_define_with_int_value ("__riscv_xlen", UNITS_PER_WORD * 8);
  if (TARGET_HARD_FLOAT)
    builtin_define_with_int_value ("__riscv_flen", UNITS_PER_FP_REG * 8);

  if ((TARGET_HARD_FLOAT || TARGET_ZFINX) && TARGET_FDIV)
    {
      builtin_define ("__riscv_fdiv");
      builtin_define ("__riscv_fsqrt");
    }

  switch (riscv_abi)
    {
    case ABI_ILP32E:
      builtin_define ("__riscv_abi_rve");
      gcc_fallthrough ();

    case ABI_ILP32:
    case ABI_LP64:
      builtin_define ("__riscv_float_abi_soft");
      break;

    case ABI_ILP32F:
    case ABI_LP64F:
      builtin_define ("__riscv_float_abi_single");
      break;

    case ABI_ILP32D:
    case ABI_LP64D:
      builtin_define ("__riscv_float_abi_double");
      break;
    }

  switch (riscv_cmodel)
    {
    case CM_MEDLOW:
      builtin_define ("__riscv_cmodel_medlow");
      break;

    case CM_PIC:
    case CM_MEDANY:
      builtin_define ("__riscv_cmodel_medany");
      break;

    }

  if (TARGET_MIN_VLEN != 0)
    builtin_define_with_int_value ("__riscv_v_min_vlen", TARGET_MIN_VLEN);

  if (TARGET_VECTOR_ELEN_64)
    builtin_define_with_int_value ("__riscv_v_elen", 64);
  else if (TARGET_VECTOR_ELEN_32)
    builtin_define_with_int_value ("__riscv_v_elen", 32);

  if (TARGET_VECTOR_ELEN_FP_64)
    builtin_define_with_int_value ("__riscv_v_elen_fp", 64);
  else if (TARGET_VECTOR_ELEN_FP_32)
    builtin_define_with_int_value ("__riscv_v_elen_fp", 32);
  else if (TARGET_MIN_VLEN != 0)
    builtin_define_with_int_value ("__riscv_v_elen_fp", 0);

  if (TARGET_MIN_VLEN)
    {
      builtin_define ("__riscv_vector");
      builtin_define_with_int_value ("__riscv_v_intrinsic",
				     riscv_ext_version_value (0, 11));
    }

  /* Define architecture extension test macros.  */
  builtin_define_with_int_value ("__riscv_arch_test", 1);

  const riscv_subset_list *subset_list = riscv_current_subset_list ();
  if (!subset_list)
    return;

  size_t max_ext_len = 0;

  /* Figure out the max length of extension name for reserving buffer.   */
  for (const riscv_subset_t *subset = subset_list->begin ();
       subset != subset_list->end ();
       subset = subset->next)
    max_ext_len = MAX (max_ext_len, subset->name.length ());

  char *buf = (char *)alloca (max_ext_len + 10 /* For __riscv_ and '\0'.  */);

  for (const riscv_subset_t *subset = subset_list->begin ();
       subset != subset_list->end ();
       subset = subset->next)
    {
      int version_value = riscv_ext_version_value (subset->major_version,
						   subset->minor_version);
      /* Special rule for zicsr and zifencei, it's used for ISA spec 2.2 or
	 earlier.  */
      if ((subset->name == "zicsr" || subset->name == "zifencei")
	  && version_value == 0)
	version_value = riscv_ext_version_value (2, 0);

      sprintf (buf, "__riscv_%s", subset->name.c_str ());
      builtin_define_with_int_value (buf, version_value);
    }
}

/* Implement "#pragma riscv intrinsic".  */

static void
riscv_pragma_intrinsic (cpp_reader *)
{
  tree x;

  if (pragma_lex (&x) != CPP_STRING)
    {
      error ("%<#pragma riscv intrinsic%> requires a string parameter");
      return;
    }

  const char *name = TREE_STRING_POINTER (x);

  if (strcmp (name, "vector") == 0)
    {
      if (!TARGET_VECTOR)
	{
	  error ("%<#pragma riscv intrinsic%> option %qs needs 'V' extension "
		 "enabled",
		 name);
	  return;
	}
      riscv_vector::handle_pragma_vector ();
    }
  else
    error ("unknown %<#pragma riscv intrinsic%> option %qs", name);
}

/* Implement TARGET_CHECK_BUILTIN_CALL.  */
static bool
riscv_check_builtin_call (location_t loc, vec<location_t> arg_loc, tree fndecl,
			  tree orig_fndecl, unsigned int nargs, tree *args)
{
  unsigned int code = DECL_MD_FUNCTION_CODE (fndecl);
  unsigned int subcode = code >> RISCV_BUILTIN_SHIFT;
  switch (code & RISCV_BUILTIN_CLASS)
    {
    case RISCV_BUILTIN_GENERAL:
      return true;

    case RISCV_BUILTIN_VECTOR:
      return riscv_vector::check_builtin_call (loc, arg_loc, subcode,
					       orig_fndecl, nargs, args);
    }
  gcc_unreachable ();
}

/* Implement REGISTER_TARGET_PRAGMAS.  */

void
riscv_register_pragmas (void)
{
  targetm.check_builtin_call = riscv_check_builtin_call;
  c_register_pragma ("riscv", "intrinsic", riscv_pragma_intrinsic);
}

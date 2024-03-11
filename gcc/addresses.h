/* Inline functions to test validity of reg classes for addressing modes.
   Copyright (C) 2006-2023 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

/* Wrapper function to unify target macros MODE_CODE_BASE_REG_CLASS,
   MODE_BASE_REG_REG_CLASS, MODE_BASE_REG_CLASS and BASE_REG_CLASS.
   Arguments as for the MODE_CODE_BASE_REG_CLASS macro.  */

#ifndef GCC_ADDRESSES_H
#define GCC_ADDRESSES_H

inline enum reg_class
base_reg_class (machine_mode mode ATTRIBUTE_UNUSED,
		addr_space_t as ATTRIBUTE_UNUSED,
		enum rtx_code outer_code ATTRIBUTE_UNUSED,
		enum rtx_code index_code ATTRIBUTE_UNUSED,
		rtx_insn *insn ATTRIBUTE_UNUSED = NULL)
{
#ifdef INSN_BASE_REG_CLASS
  return INSN_BASE_REG_CLASS (insn);
#else
#ifdef MODE_CODE_BASE_REG_CLASS
  return MODE_CODE_BASE_REG_CLASS (MACRO_MODE (mode), as, outer_code,
				   index_code);
#else
#ifdef MODE_BASE_REG_REG_CLASS
  if (index_code == REG)
    return MODE_BASE_REG_REG_CLASS (MACRO_MODE (mode));
#endif
#ifdef MODE_BASE_REG_CLASS
  return MODE_BASE_REG_CLASS (MACRO_MODE (mode));
#else
  return BASE_REG_CLASS;
#endif
#endif
#endif
}

inline enum reg_class
index_reg_class (rtx_insn *insn ATTRIBUTE_UNUSED = NULL)
{
#ifdef INSN_INDEX_REG_CLASS
  return INSN_INDEX_REG_CLASS (insn);
#else
  return INDEX_REG_CLASS;
#endif
}

/* Wrapper function to unify target macros REGNO_MODE_CODE_OK_FOR_BASE_P,
   REGNO_MODE_OK_FOR_REG_BASE_P, REGNO_MODE_OK_FOR_BASE_P and
   REGNO_OK_FOR_BASE_P.
   Arguments as for the REGNO_MODE_CODE_OK_FOR_BASE_P macro.  */

inline bool
ok_for_base_p_1 (unsigned regno ATTRIBUTE_UNUSED,
		 machine_mode mode ATTRIBUTE_UNUSED,
		 addr_space_t as ATTRIBUTE_UNUSED,
		 enum rtx_code outer_code ATTRIBUTE_UNUSED,
		 enum rtx_code index_code ATTRIBUTE_UNUSED,
		 rtx_insn* insn ATTRIBUTE_UNUSED = NULL)
{
#ifdef REGNO_OK_FOR_INSN_BASE_P
  return REGNO_OK_FOR_INSN_BASE_P (regno, insn);
#else
#ifdef REGNO_MODE_CODE_OK_FOR_BASE_P
  return REGNO_MODE_CODE_OK_FOR_BASE_P (regno, MACRO_MODE (mode), as,
					outer_code, index_code);
#else
#ifdef REGNO_MODE_OK_FOR_REG_BASE_P
  if (index_code == REG)
    return REGNO_MODE_OK_FOR_REG_BASE_P (regno, MACRO_MODE (mode));
#endif
#ifdef REGNO_MODE_OK_FOR_BASE_P
  return REGNO_MODE_OK_FOR_BASE_P (regno, MACRO_MODE (mode));
#else
  return REGNO_OK_FOR_BASE_P (regno);
#endif
#endif
#endif
}

/* Wrapper around ok_for_base_p_1, for use after register allocation is
   complete.  Arguments as for the called function.  */

inline bool
regno_ok_for_base_p (unsigned regno, machine_mode mode, addr_space_t as,
		     enum rtx_code outer_code, enum rtx_code index_code,
		     rtx_insn *insn = NULL)
{
  if (regno >= FIRST_PSEUDO_REGISTER && reg_renumber[regno] >= 0)
    regno = reg_renumber[regno];

  return ok_for_base_p_1 (regno, mode, as, outer_code, index_code, insn);
}

#endif /* GCC_ADDRESSES_H */

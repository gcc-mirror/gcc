/* Generic hooks for the RTL middle-end.
   Copyright (C) 2004 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "rtl.h"
#include "rtlhooks-def.h"
#include "expr.h"


/* For speed, we will copy the RTX hooks struct member-by-member
   instead of doing indirect calls.  For these reason, we initialize
   *two* struct rtl_hooks globals: rtl_hooks is the one that is used
   to actually call the hooks, while general_rtl_hooks is used
   to restore the hooks by passes that modify them.  */

const struct rtl_hooks general_rtl_hooks = RTL_HOOKS_INITIALIZER;
struct rtl_hooks rtl_hooks = RTL_HOOKS_INITIALIZER;

rtx
gen_lowpart_general (enum machine_mode mode, rtx x)
{
  rtx result = gen_lowpart_common (mode, x);

  if (result)
    return result;
  else if (REG_P (x))
    {
      /* Must be a hard reg that's not valid in MODE.  */
      result = gen_lowpart_common (mode, copy_to_reg (x));
      gcc_assert (result != 0);
      return result;
    }
  else
    {
      int offset = 0;

      /* The only additional case we can do is MEM.  */
      gcc_assert (MEM_P (x));

      /* The following exposes the use of "x" to CSE.  */
      if (GET_MODE_SIZE (GET_MODE (x)) <= UNITS_PER_WORD
	  && SCALAR_INT_MODE_P (GET_MODE (x))
	  && TRULY_NOOP_TRUNCATION (GET_MODE_BITSIZE (mode),
				    GET_MODE_BITSIZE (GET_MODE (x)))
	  && ! no_new_pseudos)
	return gen_lowpart_general (mode, force_reg (GET_MODE (x), x));

      if (WORDS_BIG_ENDIAN)
	offset = (MAX (GET_MODE_SIZE (GET_MODE (x)), UNITS_PER_WORD)
		  - MAX (GET_MODE_SIZE (mode), UNITS_PER_WORD));

      if (BYTES_BIG_ENDIAN)
	/* Adjust the address so that the address-after-the-data
	   is unchanged.  */
	offset -= (MIN (UNITS_PER_WORD, GET_MODE_SIZE (mode))
		   - MIN (UNITS_PER_WORD, GET_MODE_SIZE (GET_MODE (x))));

      return adjust_address (x, mode, offset);
    }
}

rtx
reg_num_sign_bit_copies_general (rtx x ATTRIBUTE_UNUSED,
				 enum machine_mode mode ATTRIBUTE_UNUSED,
                                 rtx known_x ATTRIBUTE_UNUSED,
				 enum machine_mode known_mode ATTRIBUTE_UNUSED,
                                 unsigned int known_ret ATTRIBUTE_UNUSED,
                                 unsigned int *result ATTRIBUTE_UNUSED)
{
  return NULL;
}

rtx
reg_nonzero_bits_general (rtx x ATTRIBUTE_UNUSED,
			  enum machine_mode mode ATTRIBUTE_UNUSED,
			  rtx known_x ATTRIBUTE_UNUSED,
                          enum machine_mode known_mode ATTRIBUTE_UNUSED,
                          unsigned HOST_WIDE_INT known_ret ATTRIBUTE_UNUSED,
                          unsigned HOST_WIDE_INT *nonzero ATTRIBUTE_UNUSED)
{
  return NULL;
}

/* Definitions of Toshiba Media Processor
   Copyright (C) 2001-2013 Free Software Foundation, Inc.
   Contributed by Red Hat, Inc.

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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "diagnostic-core.h"
#include "c-family/c-pragma.h"
#include "cpplib.h"
#include "hard-reg-set.h"
#include "output.h" /* for decode_reg_name */
#include "mep-protos.h"
#include "function.h"
#define MAX_RECOG_OPERANDS 10
#include "reload.h"
#include "target.h"

enum cw_which { CW_AVAILABLE, CW_CALL_SAVED };

/* This is normally provided by rtl.h but we can't include that file
   here.  It's safe to copy the definition here because we're only
   using it internally; the value isn't passed to functions outside
   this file.  */
#ifndef INVALID_REGNUM
#define INVALID_REGNUM                    (~(unsigned int) 0)
#endif

static enum cpp_ttype
mep_pragma_lex (tree *valp)
{
  enum cpp_ttype t = pragma_lex (valp);
  if (t == CPP_EOF)
    t = CPP_PRAGMA_EOL;
  return t;
}

static void
mep_pragma_io_volatile (cpp_reader *reader ATTRIBUTE_UNUSED)
{
  /* On off.  */
  tree val;
  enum cpp_ttype type;
  const char * str;

  type = mep_pragma_lex (&val);
  if (type == CPP_NAME)
    {
      str = IDENTIFIER_POINTER (val);

      type = mep_pragma_lex (&val);
      if (type != CPP_PRAGMA_EOL)
	warning (0, "junk at end of #pragma io_volatile");

      if (strcmp (str, "on") == 0)
	{
	  target_flags |= MASK_IO_VOLATILE;
	  return;
	}
      if (strcmp (str, "off") == 0)
	{
	  target_flags &= ~ MASK_IO_VOLATILE;
	  return;
	}
    }

  error ("#pragma io_volatile takes only on or off");
}

static unsigned int
parse_cr_reg (const char * str)
{
  unsigned int regno;

  regno = decode_reg_name (str);
  if (regno >= FIRST_PSEUDO_REGISTER)
    return INVALID_REGNUM;

  /* Verify that the regno is in CR_REGS.  */
  if (! TEST_HARD_REG_BIT (reg_class_contents[CR_REGS], regno))
    return INVALID_REGNUM;
  return regno;
}

static bool
parse_cr_set (HARD_REG_SET * set)
{
  tree val;
  enum cpp_ttype type;
  unsigned int last_regno = INVALID_REGNUM;
  bool do_range = false;

  CLEAR_HARD_REG_SET (*set);

  while ((type = mep_pragma_lex (&val)) != CPP_PRAGMA_EOL)
    {
      if (type == CPP_COMMA)
	{
	  last_regno = INVALID_REGNUM;
	  do_range = false;
	}
      else if (type == CPP_ELLIPSIS)
	{
	  if (last_regno == INVALID_REGNUM)
	    {
	      error ("invalid coprocessor register range");
	      return false;
	    }
	  do_range = true;
	}
      else if (type == CPP_NAME || type == CPP_STRING)
	{
	  const char *str;
	  unsigned int regno, i;

	  if (TREE_CODE (val) == IDENTIFIER_NODE)
	    str = IDENTIFIER_POINTER (val);
  	  else if (TREE_CODE (val) == STRING_CST)
	    str = TREE_STRING_POINTER (val);
	  else
	    gcc_unreachable ();

	  regno = parse_cr_reg (str);
	  if (regno == INVALID_REGNUM)
	    {
	      error ("invalid coprocessor register %qE", val);
	      return false;
	    }

	  if (do_range)
	    {
	      if (last_regno > regno)
		i = regno, regno = last_regno;
	      else
		i = last_regno;
	      do_range = false;
	    }
	  else
	    last_regno = i = regno;

	  while (i <= regno)
	    {
	      SET_HARD_REG_BIT (*set, i);
	      i++;
	    }
	}
      else
	{
	  error ("malformed coprocessor register");
	  return false;
	}
    }
  return true;
}

static void
mep_pragma_coprocessor_which (enum cw_which cw_which)
{
  HARD_REG_SET set;

  /* Process the balance of the pragma and turn it into a hard reg set.  */
  if (! parse_cr_set (&set))
    return;

  /* Process the collected hard reg set.  */
  switch (cw_which)
    {
    case CW_AVAILABLE:
      {
	int i;
	for (i = 0; i < FIRST_PSEUDO_REGISTER; ++i)
	  if (TEST_HARD_REG_BIT (set, i))
	    fixed_regs[i] = 0;
      }
      break;

    case CW_CALL_SAVED:
      {
	int i;
	for (i = 0; i < FIRST_PSEUDO_REGISTER; ++i)
	  if (TEST_HARD_REG_BIT (set, i))
	    fixed_regs[i] = call_used_regs[i] = 0;
      }
      break;

    default:
      gcc_unreachable ();
    }

  /* Fix up register class hierarchy.  */
  mep_save_register_info ();
  mep_reinit_regs ();

  if (cfun == 0)
    {
      init_dummy_function_start ();
      init_caller_save ();
      expand_dummy_function_end ();
    }
  else
    {
      init_caller_save ();
    }
}

static void
mep_pragma_coprocessor_width (void)
{
  tree val;
  enum cpp_ttype type;
  HOST_WIDE_INT i;

  type = mep_pragma_lex (&val);
  switch (type)
    {
    case CPP_NUMBER:
      if (! tree_fits_uhwi_p (val))
	break;
      i = tree_to_uhwi (val);
      /* This pragma no longer has any effect.  */
#if 0
      if (i == 32)
	target_flags &= ~MASK_64BIT_CR_REGS;
      else if (i == 64)
	target_flags |= MASK_64BIT_CR_REGS;
      else
	break;
      targetm.init_builtins ();
#else
      if (i != 32 && i != 64)
	break;
#endif

      type = mep_pragma_lex (&val);
      if (type != CPP_PRAGMA_EOL)
	warning (0, "junk at end of #pragma GCC coprocessor width");
      return;

    default:
      break;
    }

  error ("#pragma GCC coprocessor width takes only 32 or 64");
}

static void
mep_pragma_coprocessor_subclass (void)
{
  tree val;
  enum cpp_ttype type;
  HARD_REG_SET set;
  int class_letter;
  enum reg_class rclass;

  type = mep_pragma_lex (&val);
  if (type != CPP_CHAR)
    goto syntax_error;
  class_letter = tree_to_uhwi (val);
  if (class_letter >= 'A' && class_letter <= 'D')
    switch (class_letter)
      {
      case 'A':
	rclass = USER0_REGS;
	break;
      case 'B':
	rclass = USER1_REGS;
	break;
      case 'C':
	rclass = USER2_REGS;
	break;
      case 'D':
	rclass = USER3_REGS;
	break;
      }
  else
    {
      error ("#pragma GCC coprocessor subclass letter must be in [ABCD]");
      return;
    }
  if (reg_class_size[rclass] > 0)
    {
      error ("#pragma GCC coprocessor subclass '%c' already defined",
	     class_letter);
      return;
    }

  type = mep_pragma_lex (&val);
  if (type != CPP_EQ)
    goto syntax_error;

  if (! parse_cr_set (&set))
    return;

  /* Fix up register class hierarchy.  */
  COPY_HARD_REG_SET (reg_class_contents[rclass], set);
  mep_init_regs ();
  return;

 syntax_error:
  error ("malformed #pragma GCC coprocessor subclass");
}

static void
mep_pragma_disinterrupt (cpp_reader *reader ATTRIBUTE_UNUSED)
{
  tree val;
  enum cpp_ttype type;
  int saw_one = 0;

  for (;;)
    {
      type = mep_pragma_lex (&val);
      if (type == CPP_COMMA)
	continue;
      if (type != CPP_NAME)
	break;
      mep_note_pragma_disinterrupt (IDENTIFIER_POINTER (val));
      saw_one = 1;
    }
  if (!saw_one || type != CPP_PRAGMA_EOL)
    {
      error ("malformed #pragma disinterrupt");
      return;
    }
}

static void
mep_pragma_coprocessor (cpp_reader *reader ATTRIBUTE_UNUSED)
{
  tree val;
  enum cpp_ttype type;

  type = mep_pragma_lex (&val);
  if (type != CPP_NAME)
    {
      error ("malformed #pragma GCC coprocessor");
      return;
    }

  if (!TARGET_COP)
    error ("coprocessor not enabled");

  if (strcmp (IDENTIFIER_POINTER (val), "available") == 0)
    mep_pragma_coprocessor_which (CW_AVAILABLE);
  else if (strcmp (IDENTIFIER_POINTER (val), "call_saved") == 0)
    mep_pragma_coprocessor_which (CW_CALL_SAVED);
  else if (strcmp (IDENTIFIER_POINTER (val), "width") == 0)
    mep_pragma_coprocessor_width ();
  else if (strcmp (IDENTIFIER_POINTER (val), "subclass") == 0)
    mep_pragma_coprocessor_subclass ();
  else
    error ("unknown #pragma GCC coprocessor %E", val);
}

static void
mep_pragma_call (cpp_reader *reader ATTRIBUTE_UNUSED)
{
  tree val;
  enum cpp_ttype type;
  int saw_one = 0;

  for (;;)
    {
      type = mep_pragma_lex (&val);
      if (type == CPP_COMMA)
	continue;
      if (type != CPP_NAME)
	break;
      mep_note_pragma_call (IDENTIFIER_POINTER (val));
      saw_one = 1;
    }
  if (!saw_one || type != CPP_PRAGMA_EOL)
    {
      error ("malformed #pragma call");
      return;
    }
}

void
mep_register_pragmas (void)
{
  c_register_pragma ("custom", "io_volatile", mep_pragma_io_volatile);
  c_register_pragma ("GCC", "coprocessor", mep_pragma_coprocessor);
  c_register_pragma (0, "disinterrupt", mep_pragma_disinterrupt);
  c_register_pragma (0, "call", mep_pragma_call);
}

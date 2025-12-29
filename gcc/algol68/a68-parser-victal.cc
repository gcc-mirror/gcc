/* Syntax check for formal, actual and virtual declarers.
   Copyright (C) 2001-2023 J. Marcel van der Veer.
   Copyright (C) 2025 Jose E. Marchesi.

   Original implementation by J. Marcel van der Veer.
   Adapted for GCC by Jose E. Marchesi.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "options.h"

#include "a68.h"

static bool victal_check_declarer (NODE_T *, int);

/* Check generator.  */

static void
victal_check_generator (NODE_T * p)
{
  if (!victal_check_declarer (NEXT (p), ACTUAL_DECLARER_MARK))
    a68_error (p, "Y expected", "actual declarer");
}

/* Check formal pack.  */

static void
victal_check_formal_pack (NODE_T *p, int x, bool *z)
{
  if (p != NO_NODE)
    {
      if (IS (p, FORMAL_DECLARERS))
	victal_check_formal_pack (SUB (p), x, z);
      else if (a68_is_one_of (p, OPEN_SYMBOL, COMMA_SYMBOL, STOP))
	victal_check_formal_pack (NEXT (p), x, z);
      else if (IS (p, FORMAL_DECLARERS_LIST))
	{
	  victal_check_formal_pack (NEXT (p), x, z);
	  victal_check_formal_pack (SUB (p), x, z);
	}
      else if (IS (p, DECLARER))
	{
	  victal_check_formal_pack (NEXT (p), x, z);
	  (*z) &= victal_check_declarer (SUB (p), x);
	}
    }
}

/* Check operator declaration.  */

static void
victal_check_operator_dec (NODE_T *p)
{
  if (IS (NEXT (p), FORMAL_DECLARERS))
    {
      bool z = true;
      victal_check_formal_pack (NEXT (p), FORMAL_DECLARER_MARK, &z);
      if (!z)
	a68_error (p, "Y expected", "formal declarers");
      FORWARD (p);
  }
  if (!victal_check_declarer (NEXT (p), FORMAL_DECLARER_MARK))
    a68_error (p, "Y expected", "formal declarer");
}

/* Check mode declaration.  */

static void
victal_check_mode_dec (NODE_T *p)
{
  if (p != NO_NODE)
    {
      if (IS (p, MODE_DECLARATION))
	{
	  victal_check_mode_dec (SUB (p));
	  victal_check_mode_dec (NEXT (p));
	}
      else if (IS (p, PUBLIC_SYMBOL))
	{
	  victal_check_mode_dec (NEXT (p));
	}
      else if (a68_is_one_of (p, MODE_SYMBOL, DEFINING_INDICANT, STOP)
               || a68_is_one_of (p, EQUALS_SYMBOL, COMMA_SYMBOL, STOP))
	{
	  victal_check_mode_dec (NEXT (p));
	}
      else if (IS (p, DECLARER))
	{
	  if (!victal_check_declarer (p, ACTUAL_DECLARER_MARK))
	    a68_error (p, "Y expected", "actual declarer");
	}
    }
}

/* Check variable declaration. */

static void
victal_check_variable_dec (NODE_T *p)
{
  if (p != NO_NODE)
    {
      if (IS (p, VARIABLE_DECLARATION))
	{
	  victal_check_variable_dec (SUB (p));
	  victal_check_variable_dec (NEXT (p));
	}
      else
	{
	  if (IS (p, PUBLIC_SYMBOL))
	    FORWARD (p);

	  if (IS (p, QUALIFIER))
	    FORWARD (p);

	  if (a68_is_one_of (p, DEFINING_IDENTIFIER, ASSIGN_SYMBOL, STOP)
	      || IS (p, COMMA_SYMBOL))
	    victal_check_variable_dec (NEXT (p));
	  else if (IS (p, UNIT))
	    a68_victal_checker (SUB (p));
	  else if (IS (p, DECLARER))
	    {
	      if (!victal_check_declarer (p, ACTUAL_DECLARER_MARK))
		a68_error (p, "Y expected", "actual declarer");
	      victal_check_variable_dec (NEXT (p));
	    }
	}
    }
}

/* Check identity declaration.  */

static void
victal_check_identity_dec (NODE_T * p)
{
  if (p != NO_NODE)
    {
      if (IS (p, IDENTITY_DECLARATION))
	{
	  victal_check_identity_dec (SUB (p));
	  victal_check_identity_dec (NEXT (p));
	}
      else if (a68_is_one_of (p, PUBLIC_SYMBOL, DEFINING_IDENTIFIER, EQUALS_SYMBOL, COMMA_SYMBOL,
			      STOP))
	victal_check_identity_dec (NEXT (p));
      else if (IS (p, UNIT))
	a68_victal_checker (SUB (p));
      else if (IS (p, DECLARER))
	{
	  if (!victal_check_declarer (p, FORMAL_DECLARER_MARK))
	    a68_error (p, "Y expected", "formal declarer");
	  victal_check_identity_dec (NEXT (p));
	}
    }
}

/* Check routine pack.  */

static void
victal_check_routine_pack (NODE_T *p, int x, bool *z)
{
  if (p != NO_NODE)
    {
      if (IS (p, PARAMETER_PACK))
	victal_check_routine_pack (SUB (p), x, z);
      else if (a68_is_one_of (p, OPEN_SYMBOL, COMMA_SYMBOL, STOP))
	victal_check_routine_pack (NEXT (p), x, z);
      else if (a68_is_one_of (p, PARAMETER_LIST, PARAMETER, STOP))
	{
	  victal_check_routine_pack (NEXT (p), x, z);
	  victal_check_routine_pack (SUB (p), x, z);
	}
      else if (IS (p, DECLARER))
	*z &= victal_check_declarer (SUB (p), x);
    }
}

/* Check routine text.  */

static void
victal_check_routine_text (NODE_T *p)
{
  if (IS (p, PARAMETER_PACK))
    {
      bool z = true;
      victal_check_routine_pack (p, FORMAL_DECLARER_MARK, &z);
      if (!z)
	a68_error (p, "Y expected", "formal declarers");
      FORWARD (p);
    }
  if (!victal_check_declarer (p, FORMAL_DECLARER_MARK))
    a68_error (p, "Y expected", "formal declarer");
  a68_victal_checker (NEXT (p));
}

/* Check structure pack.  */

static void
victal_check_structure_pack (NODE_T *p, int x, bool *z)
{
  if (p != NO_NODE)
    {
      if (IS (p, STRUCTURE_PACK))
	victal_check_structure_pack (SUB (p), x, z);
      else if (a68_is_one_of (p, OPEN_SYMBOL, COMMA_SYMBOL, STOP))
	victal_check_structure_pack (NEXT (p), x, z);
      else if (a68_is_one_of (p, STRUCTURED_FIELD_LIST, STRUCTURED_FIELD, STOP))
	{
	  victal_check_structure_pack (NEXT (p), x, z);
	  victal_check_structure_pack (SUB (p), x, z);
	}
      else if (IS (p, DECLARER))
	(*z) &= victal_check_declarer (SUB (p), x);
    }
}

/* Check union pack.  */

static void
victal_check_union_pack (NODE_T * p, int x, bool * z)
{
  if (p != NO_NODE)
    {
    if (IS (p, UNION_PACK))
      victal_check_union_pack (SUB (p), x, z);
    else if (a68_is_one_of (p, OPEN_SYMBOL, COMMA_SYMBOL, VOID_SYMBOL, STOP))
      victal_check_union_pack (NEXT (p), x, z);
    else if (IS (p, UNION_DECLARER_LIST))
      {
	victal_check_union_pack (NEXT (p), x, z);
	victal_check_union_pack (SUB (p), x, z);
      }
    else if (IS (p, DECLARER))
      {
	victal_check_union_pack (NEXT (p), x, z);
	(*z) &= victal_check_declarer (SUB (p), FORMAL_DECLARER_MARK);
      }
    }
}

/* Check declarer.  */

static bool
victal_check_declarer (NODE_T *p, int x)
{
  if (p == NO_NODE)
    return false;
  else if (IS (p, DECLARER))
    return victal_check_declarer (SUB (p), x);
  else if (a68_is_one_of (p, LONGETY, SHORTETY, STOP))
    return true;
  else if (a68_is_one_of (p, VOID_SYMBOL, INDICANT, STANDARD, STOP))
    return true;
  else if (IS_REF (p))
    return victal_check_declarer (NEXT (p), VIRTUAL_DECLARER_MARK);
  else if (IS_FLEX (p))
    return victal_check_declarer (NEXT (p), x);
  else if (IS (p, BOUNDS))
    {
      a68_victal_checker (SUB (p));
      if (x == FORMAL_DECLARER_MARK)
	{
	  a68_error (p, "Y expected", "formal bounds");
	  (void) victal_check_declarer (NEXT (p), x);
	  return true;
	}
      else if (x == VIRTUAL_DECLARER_MARK)
	{
	  a68_error (p, "Y expected", "virtual bounds");
	  (void) victal_check_declarer (NEXT (p), x);
	  return true;
	}
      else
	return victal_check_declarer (NEXT (p), x);
    }
  else if (IS (p, FORMAL_BOUNDS))
    {
      a68_victal_checker (SUB (p));
      if (x == ACTUAL_DECLARER_MARK)
	{
	  a68_error (p, "Y expected", "actual bounds");
	  (void) victal_check_declarer (NEXT (p), x);
	  return true;
	}
      else
	return victal_check_declarer (NEXT (p), x);
    }
  else if (IS (p, STRUCT_SYMBOL))
    {
      bool z = true;
      victal_check_structure_pack (NEXT (p), x, &z);
      return z;
    }
  else if (IS (p, UNION_SYMBOL))
    {
      bool z = true;
      victal_check_union_pack (NEXT (p), FORMAL_DECLARER_MARK, &z);
      if (!z)
	a68_error (p, "Y expected", "formal declarer pack");
      return true;
    }
  else if (IS (p, PROC_SYMBOL))
    {
      if (IS (NEXT (p), FORMAL_DECLARERS))
	{
	  bool z = true;
	  victal_check_formal_pack (NEXT (p), FORMAL_DECLARER_MARK, &z);
	  if (!z)
	    a68_error (p, "Y expected", "formal declarer");
	  FORWARD (p);
	}
      if (!victal_check_declarer (NEXT (p), FORMAL_DECLARER_MARK))
	a68_error (p, "Y expected", "formal declarer");
      return true;
    }
  else
    return false;
}

/* Check cast.  */

static void
victal_check_cast (NODE_T *p)
{
  if (!victal_check_declarer (p, FORMAL_DECLARER_MARK))
    {
      a68_error (p, "Y expected", "formal declarer");
      a68_victal_checker (NEXT (p));
    }
}

/* Driver for checking VICTALITY of declarers.  */

void
a68_victal_checker (NODE_T *p)
{
  for (; p != NO_NODE; FORWARD (p))
    {
      if (IS (p, MODE_DECLARATION))
	victal_check_mode_dec (SUB (p));
      else if (IS (p, VARIABLE_DECLARATION))
	victal_check_variable_dec (SUB (p));
      else if (IS (p, IDENTITY_DECLARATION))
	victal_check_identity_dec (SUB (p));
      else if (IS (p, GENERATOR))
	victal_check_generator (SUB (p));
      else if (IS (p, ROUTINE_TEXT))
	victal_check_routine_text (SUB (p));
      else if (IS (p, OPERATOR_PLAN))
	victal_check_operator_dec (SUB (p));
      else if (IS (p, CAST))
	victal_check_cast (SUB (p));
      else
	a68_victal_checker (SUB (p));
    }
}

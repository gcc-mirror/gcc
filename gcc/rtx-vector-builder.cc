/* A class for building vector rtx constants.
   Copyright (C) 2017-2024 Free Software Foundation, Inc.

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
#include "rtl.h"
#include "rtx-vector-builder.h"

/* Return a CONST_VECTOR for the current constant.  V is an existing
   rtvec that contains all the elements.  */

rtx
rtx_vector_builder::build (rtvec v)
{
  finalize ();

  rtx x = find_cached_value ();
  if (x)
    return x;

  x = gen_rtx_raw_CONST_VECTOR (m_mode, v);
  CONST_VECTOR_NPATTERNS (x) = npatterns ();
  CONST_VECTOR_NELTS_PER_PATTERN (x) = nelts_per_pattern ();
  return x;
}

/* Return a vector element with the value BASE + FACTOR * STEP.  */

rtx
rtx_vector_builder::apply_step (rtx base, unsigned int factor,
				const poly_wide_int &step) const
{
  scalar_int_mode int_mode = as_a <scalar_int_mode> (GET_MODE_INNER (m_mode));
  return immed_wide_int_const (wi::to_poly_wide (base, int_mode)
			       + factor * step,
			       int_mode);
}

/* Return a CONST_VECTOR for the current constant.  */

rtx
rtx_vector_builder::build ()
{
  finalize ();

  rtx x = find_cached_value ();
  if (x)
    return x;

  unsigned int nelts;
  if (!GET_MODE_NUNITS (m_mode).is_constant (&nelts))
    nelts = encoded_nelts ();
  rtvec v = rtvec_alloc (nelts);
  for (unsigned int i = 0; i < nelts; ++i)
    RTVEC_ELT (v, i) = elt (i);
  x = gen_rtx_raw_CONST_VECTOR (m_mode, v);
  CONST_VECTOR_NPATTERNS (x) = npatterns ();
  CONST_VECTOR_NELTS_PER_PATTERN (x) = nelts_per_pattern ();
  return x;
}

/* Check whether there is a global cached value for the vector.
   Return it if so, otherwise return null.  */

rtx
rtx_vector_builder::find_cached_value ()
{
  if (encoded_nelts () != 1)
    return NULL_RTX;

  rtx elt = (*this)[0];

  if (GET_MODE_CLASS (m_mode) == MODE_VECTOR_BOOL)
    {
      if (elt == const1_rtx)
	return CONST1_RTX (m_mode);
      else if (elt == constm1_rtx)
	return CONSTM1_RTX (m_mode);
      else if (elt == const0_rtx)
	return CONST0_RTX (m_mode);
      else
	gcc_unreachable ();
    }

  /* We can be called before the global vector constants are set up,
     but in that case we'll just return null.  */
  scalar_mode inner_mode = GET_MODE_INNER (m_mode);
  if (elt == CONST0_RTX (inner_mode))
    return CONST0_RTX (m_mode);
  else if (elt == CONST1_RTX (inner_mode))
    return CONST1_RTX (m_mode);
  else if (elt == CONSTM1_RTX (inner_mode))
    return CONSTM1_RTX (m_mode);

  return NULL_RTX;
}

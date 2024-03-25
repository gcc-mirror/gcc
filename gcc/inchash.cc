/* Incremential hashing for jhash.
   Copyright (C) 2014-2024 Free Software Foundation, Inc.

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

#ifdef GENERATOR_FILE
#include "bconfig.h"
#else
#include "config.h"
#endif
#include "system.h"
#include "coretypes.h"
#include "real.h"
#include "inchash.h"

namespace inchash
{

/* This is here instead of inchash.h to keep us from having to put
   real.h in coretypes.h.  */
void
hash::add_real_value (const real_value &v)
{
  add_int (v.cl);
  add_int (v.sign);
  switch (v.cl)
    {
    case rvc_zero:
    case rvc_inf:
      return;
    case rvc_normal:
      add_int (v.decimal);
      add_int (REAL_EXP (&v));
      break;
    case rvc_nan:
      add_int (v.signalling);
      add_int (v.canonical);
      if (v.canonical)
	return;
      break;
    default:
      gcc_unreachable ();
    }
  for (unsigned i = 0; i < SIGSZ; ++i)
    add_hwi (v.sig[i]);
}

} // namespace inchash

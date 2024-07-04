/* Definitions of floating-point conversion from compiler
   internal format to MPFR.
   Copyright (C) 2010-2024 Free Software Foundation, Inc.

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

#ifndef GCC_REALGMP_H
#define GCC_REALGMP_H

#include <mpfr.h>
#include <mpc.h>

class auto_mpfr
{
public:
  auto_mpfr () { mpfr_init (m_mpfr); }
  explicit auto_mpfr (mpfr_prec_t prec) { mpfr_init2 (m_mpfr, prec); }
  ~auto_mpfr () { mpfr_clear (m_mpfr); }

  operator mpfr_t& () { return m_mpfr; }
  mpfr_ptr operator-> () { return m_mpfr; }

  auto_mpfr (const auto_mpfr &) = delete;
  auto_mpfr &operator= (const auto_mpfr &) = delete;

#if GCC_VERSION < 4008 || GCC_VERSION >= 5000
  /* GCC 4.8 and 4.9 don't support this, only fixed in PR62101 for 5.0.  */
  friend void mpfr_clear (auto_mpfr&) = delete;
  friend void mpfr_init (auto_mpfr&) = delete;
  friend void mpfr_init2 (auto_mpfr&, mpfr_prec_t) = delete;
#endif

private:
  mpfr_t m_mpfr;
};

/* Convert between MPFR and REAL_VALUE_TYPE.  The caller is
   responsible for initializing and clearing the MPFR parameter.  */

extern void real_from_mpfr (REAL_VALUE_TYPE *, mpfr_srcptr, tree, mpfr_rnd_t);
extern void real_from_mpfr (REAL_VALUE_TYPE *, mpfr_srcptr,
			    const real_format *, mpfr_rnd_t);
extern void mpfr_from_real (mpfr_ptr, const REAL_VALUE_TYPE *, mpfr_rnd_t);

#endif /* ! GCC_REALGMP_H */

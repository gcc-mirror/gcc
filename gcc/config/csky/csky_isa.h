/* ISA feature enumerations for C-SKY targets.
   Copyright (C) 2018 Free Software Foundation, Inc.
   Contributed by C-SKY Microsystems and Mentor Graphics.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 3, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#ifndef GCC_CSKY_ISA_FEATURE_H
#define GCC_CSKY_ISA_FEATURE_H


#ifndef CSKY_ISA_MACRO
#define CSKY_ISA_MACRO
#endif

#define CSKY_ISA_FEATURE_DEFINE(x)  isa_bit_ ## x
#define CSKY_ISA_FEATURE_GET(x)	    CSKY_ISA_FEATURE_DEFINE (x)

enum csky_isa_feature
  {
    CSKY_ISA_FEATURE_DEFINE (none),
#undef	CSKY_ISA
#define CSKY_ISA(IDENT, DESC) \
    CSKY_ISA_FEATURE_DEFINE (IDENT),
#include "csky_isa.def"
#undef	CSKY_ISA
    CSKY_ISA_FEATURE_DEFINE (max)
  };

#define CSKY_ISA_FEAT(x) x,
#define CSKY_ISA_FEAT_NONE CSKY_ISA_FEAT (isa_bit_none)


#endif /* GCC_CSKY_ISA_FEATURE_H */

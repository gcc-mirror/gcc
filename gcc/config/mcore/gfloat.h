/* Output routines for Motorola MCore processor
   Copyright (C) 1993, 1999, 2000 Free Software Foundation, Inc.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* float.h for the M*Core microprocessor. It uses IEEE floating point.
 *	float is 32 bit IEEE-754 format
 *	double is 64 bit IEEE-754 format
 *	long double is not defined right now...
 */
#ifndef	__FLOAT_H___
#define	__FLOAT_H___

#define FLT_RADIX 2
#define FLT_ROUNDS 1

#define FLT_MANT_DIG	24
#define FLT_DIG       	6
#define FLT_EPSILON 	((float)1.19209290e-07)
#define FLT_MIN_EXP 	(-125)
#define FLT_MIN 	((float)1.17549435e-38)
#define FLT_MIN_10_EXP 	(-37)
#define FLT_MAX_EXP 	128
#define FLT_MAX 	((float)3.40282347e+38)
#define FLT_MAX_10_EXP 	38

#define DBL_MANT_DIG 	53
#define DBL_DIG 	15
#define DBL_EPSILON 	2.2204460492503131e-16
#define DBL_MIN_EXP 	(-1021)
#define DBL_MIN 	2.2250738585072014e-308
#define DBL_MIN_10_EXP 	(-307)
#define DBL_MAX_EXP 	1024
#define DBL_MAX 	1.7976931348623157e+308
#define DBL_MAX_10_EXP 	308


/* No definitions for LDBL at this time.  */

#undef	LDBL_MANT_DIG
#undef	LDBL_DIG
#undef	LDBL_EPSILON
#undef	LDBL_MIN_EXP
#undef	LDBL_MIN
#undef	LDBL_MIN_10_EXP
#undef	LDBL_MAX_EXP
#undef	LDBL_MAX
#undef	LDBL_MAX_10_EXP

#endif /* __FLOAT_H__ */

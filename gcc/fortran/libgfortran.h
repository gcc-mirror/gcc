/* Header file to the Fortran front-end and runtime library
   Copyright (C) 2007, 2008, 2009 Free Software Foundation, Inc.

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


/* Flags to specify which standard/extension contains a feature.
   Note that no features were obsoleted nor deleted in F2003.
   Please remember to keep those definitions in sync with
   gfortran.texi.  */
#define GFC_STD_F2008	(1<<7)	/* New in F2008.  */
#define GFC_STD_LEGACY	(1<<6)	/* Backward compatibility.  */
#define GFC_STD_GNU	(1<<5)	/* GNU Fortran extension.  */
#define GFC_STD_F2003	(1<<4)	/* New in F2003.  */
#define GFC_STD_F95	(1<<3)	/* New in F95.  */
#define GFC_STD_F95_DEL	(1<<2)	/* Deleted in F95.  */
#define GFC_STD_F95_OBS	(1<<1)	/* Obsolescent in F95.  */
#define GFC_STD_F77	(1<<0)	/* Included in F77, but not deleted or
				   obsolescent in later standards.  */


/* Bitmasks for the various FPE that can be enabled.  */
#define GFC_FPE_INVALID    (1<<0)
#define GFC_FPE_DENORMAL   (1<<1)
#define GFC_FPE_ZERO       (1<<2)
#define GFC_FPE_OVERFLOW   (1<<3)
#define GFC_FPE_UNDERFLOW  (1<<4)
#define GFC_FPE_PRECISION  (1<<5)


/* Bitmasks for the various runtime checks that can be enabled.  */
#define GFC_RTCHECK_BOUNDS      (1<<0)
#define GFC_RTCHECK_ARRAY_TEMPS (1<<1)
#define GFC_RTCHECK_RECURSION   (1<<2)
#define GFC_RTCHECK_DO          (1<<3)
#define GFC_RTCHECK_POINTER     (1<<4)
#define GFC_RTCHECK_MEM         (1<<5)
#define GFC_RTCHECK_ALL        (GFC_RTCHECK_BOUNDS | GFC_RTCHECK_ARRAY_TEMPS \
				| GFC_RTCHECK_RECURSION | GFC_RTCHECK_DO \
				| GFC_RTCHECK_POINTER | GFC_RTCHECK_MEM)


/* Possible values for the CONVERT I/O specifier.  */
typedef enum
{
  GFC_CONVERT_NONE = -1,
  GFC_CONVERT_NATIVE = 0,
  GFC_CONVERT_SWAP,
  GFC_CONVERT_BIG,
  GFC_CONVERT_LITTLE
}
unit_convert;


/* Runtime errors.  */
typedef enum
{
  LIBERROR_FIRST = -3,		/* Marker for the first error.  */
  LIBERROR_EOR = -2,		/* End of record, must be negative.  */
  LIBERROR_END = -1,		/* End of file, must be negative.  */
  LIBERROR_OK = 0,		/* Indicates success, must be zero.  */
  LIBERROR_OS = 5000,		/* OS error, more info in errno.  */
  LIBERROR_OPTION_CONFLICT,
  LIBERROR_BAD_OPTION,
  LIBERROR_MISSING_OPTION,
  LIBERROR_ALREADY_OPEN,
  LIBERROR_BAD_UNIT,
  LIBERROR_FORMAT,
  LIBERROR_BAD_ACTION,
  LIBERROR_ENDFILE,
  LIBERROR_BAD_US,
  LIBERROR_READ_VALUE,
  LIBERROR_READ_OVERFLOW,
  LIBERROR_INTERNAL,
  LIBERROR_INTERNAL_UNIT,
  LIBERROR_ALLOCATION,
  LIBERROR_DIRECT_EOR,
  LIBERROR_SHORT_RECORD,
  LIBERROR_CORRUPT_FILE,
  LIBERROR_LAST			/* Not a real error, the last error # + 1.  */
}
libgfortran_error_codes;


/* Default unit number for preconnected standard input and output.  */
#define GFC_STDIN_UNIT_NUMBER 5
#define GFC_STDOUT_UNIT_NUMBER 6
#define GFC_STDERR_UNIT_NUMBER 0


/* FIXME: Increase to 15 for Fortran 2008. Also needs changes to
   GFC_DTYPE_RANK_MASK. See PR 36825.  */
#define GFC_MAX_DIMENSIONS 7

#define GFC_DTYPE_RANK_MASK 0x07
#define GFC_DTYPE_TYPE_SHIFT 3
#define GFC_DTYPE_TYPE_MASK 0x38
#define GFC_DTYPE_SIZE_SHIFT 6

typedef enum
{
  GFC_DTYPE_UNKNOWN = 0,
  GFC_DTYPE_INTEGER,
  /* TODO: recognize logical types.  */
  GFC_DTYPE_LOGICAL,
  GFC_DTYPE_REAL,
  GFC_DTYPE_COMPLEX,
  GFC_DTYPE_DERIVED,
  GFC_DTYPE_CHARACTER
}
dtype;


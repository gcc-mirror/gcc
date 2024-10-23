/* Header file to the Fortran front-end and runtime library
   Copyright (C) 2007-2024 Free Software Foundation, Inc.

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
   Note that no features were obsoleted nor deleted in F2003 nor in F2023.
   Nevertheless, some features available in F2018 are prohibited in F2023.
   Please remember to keep those definitions in sync with
   gfortran.texi.  */
#define GFC_STD_F202Y		(1<<14)	/* Enable proposed F202y features.  */
#define GFC_STD_UNSIGNED	(1<<14) /* Not really a standard, but
					   better for error handling.  */
#define GFC_STD_F2023_DEL	(1<<13)	/* Prohibited in F2023.  */
#define GFC_STD_F2023		(1<<12)	/* New in F2023.  */
#define GFC_STD_F2018_DEL	(1<<11)	/* Deleted in F2018.  */
#define GFC_STD_F2018_OBS	(1<<10)	/* Obsolescent in F2018.  */
#define GFC_STD_F2018		(1<<9)	/* New in F2018.  */
#define GFC_STD_F2008_OBS	(1<<8)	/* Obsolescent in F2008.  */
#define GFC_STD_F2008		(1<<7)	/* New in F2008.  */
#define GFC_STD_LEGACY		(1<<6)	/* Backward compatibility.  */
#define GFC_STD_GNU		(1<<5)	/* GNU Fortran extension.  */
#define GFC_STD_F2003		(1<<4)	/* New in F2003.  */
#define GFC_STD_F95		(1<<3)	/* New in F95.  */
#define GFC_STD_F95_DEL		(1<<2)	/* Deleted in F95.  */
#define GFC_STD_F95_OBS		(1<<1)	/* Obsolescent in F95.  */
#define GFC_STD_F77		(1<<0)	/* Included in F77, but not deleted or
					   obsolescent in later standards.  */

/* Combinations of the above flags that specify which classes of features
 * are allowed with a certain -std option.  */
#define GFC_STD_OPT_F95		(GFC_STD_F77 | GFC_STD_F95 | GFC_STD_F95_OBS  \
				| GFC_STD_F2008_OBS | GFC_STD_F2018_OBS \
				| GFC_STD_F2018_DEL | GFC_STD_F2023_DEL)
#define GFC_STD_OPT_F03		(GFC_STD_OPT_F95 | GFC_STD_F2003)
#define GFC_STD_OPT_F08		(GFC_STD_OPT_F03 | GFC_STD_F2008)
#define GFC_STD_OPT_F18		((GFC_STD_OPT_F08 | GFC_STD_F2018) \
				& (~GFC_STD_F2018_DEL))
#define GFC_STD_OPT_F23		((GFC_STD_OPT_F18 | GFC_STD_F2023) \
				& (~GFC_STD_F2023_DEL))

/* Bitmasks for the various FPE that can be enabled.  These need to be straight integers
   e.g., 8 instead of (1<<3), because they will be included in Fortran source.  */
#define GFC_FPE_INVALID      1
#define GFC_FPE_DENORMAL     2
#define GFC_FPE_ZERO         4
#define GFC_FPE_OVERFLOW     8
#define GFC_FPE_UNDERFLOW   16
#define GFC_FPE_INEXACT     32

/* Defines for floating-point rounding modes.  */
#define GFC_FPE_DOWNWARD   1
#define GFC_FPE_TONEAREST  2
#define GFC_FPE_TOWARDZERO 3
#define GFC_FPE_UPWARD     4
#define GFC_FPE_AWAY       5

/* Size of the buffer required to store FPU state for any target.
   In particular, this has to be larger than fenv_t on all glibc targets.
   Currently, the winner is x86_64 with 32 bytes.  */
#define GFC_FPE_STATE_BUFFER_SIZE 32

/* Bitmasks for the various runtime checks that can be enabled.  */
#define GFC_RTCHECK_BOUNDS      (1<<0)
#define GFC_RTCHECK_ARRAY_TEMPS (1<<1)
#define GFC_RTCHECK_RECURSION   (1<<2)
#define GFC_RTCHECK_DO          (1<<3)
#define GFC_RTCHECK_POINTER     (1<<4)
#define GFC_RTCHECK_MEM         (1<<5)
#define GFC_RTCHECK_BITS        (1<<6)
#define GFC_RTCHECK_ALL        (GFC_RTCHECK_BOUNDS | GFC_RTCHECK_ARRAY_TEMPS \
				| GFC_RTCHECK_RECURSION | GFC_RTCHECK_DO \
				| GFC_RTCHECK_POINTER | GFC_RTCHECK_MEM \
				| GFC_RTCHECK_BITS)

/* Special unit numbers used to convey certain conditions.  Numbers -4
   thru -9 available.  NEWUNIT values start at -10.  */
#define GFC_INTERNAL_UNIT  -1    /* KIND=1 Internal Unit.  */
#define GFC_INTERNAL_UNIT4 -2    /* KIND=4 Internal Unit.  */
#define GFC_INVALID_UNIT   -3

/* Possible values for the CONVERT I/O specifier.  */
/* Keep in sync with GFC_FLAG_CONVERT_* in gcc/flag-types.h.  */
typedef enum
{
  GFC_CONVERT_NONE = -1,
  GFC_CONVERT_NATIVE = 0,
  GFC_CONVERT_SWAP,
  GFC_CONVERT_BIG,
  GFC_CONVERT_LITTLE,
  GFC_CONVERT_R16_IEEE = 4,
  GFC_CONVERT_R16_IEEE_SWAP,
  GFC_CONVERT_R16_IEEE_BIG,
  GFC_CONVERT_R16_IEEE_LITTLE,
  GFC_CONVERT_R16_IBM = 8,
  GFC_CONVERT_R16_IBM_SWAP,
  GFC_CONVERT_R16_IBM_BIG,
  GFC_CONVERT_R16_IBM_LITTLE,
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
  LIBERROR_INQUIRE_INTERNAL_UNIT, /* Must be different from STAT_STOPPED_IMAGE.  */
  LIBERROR_BAD_WAIT_ID,
  LIBERROR_NO_MEMORY,
  LIBERROR_LAST			/* Not a real error, the last error # + 1.  */
}
libgfortran_error_codes;

/* Must kept in sync with libgfortran/caf/libcaf.h.  */
typedef enum
{
  GFC_STAT_UNLOCKED = 0,
  GFC_STAT_LOCKED,
  GFC_STAT_LOCKED_OTHER_IMAGE,
  GFC_STAT_STOPPED_IMAGE = 6000, /* See LIBERROR_INQUIRE_INTERNAL_UNIT above. */
  GFC_STAT_FAILED_IMAGE  = 6001
}
libgfortran_stat_codes;

typedef enum
{
  GFC_CAF_ATOMIC_ADD = 1,
  GFC_CAF_ATOMIC_AND,
  GFC_CAF_ATOMIC_OR,
  GFC_CAF_ATOMIC_XOR
} libcaf_atomic_codes;


/* For CO_REDUCE.  */
#define GFC_CAF_BYREF      (1<<0)
#define GFC_CAF_HIDDENLEN  (1<<1)
#define GFC_CAF_ARG_VALUE  (1<<2)
#define GFC_CAF_ARG_DESC   (1<<3)


/* Default unit number for preconnected standard input and output.  */
#define GFC_STDIN_UNIT_NUMBER 5
#define GFC_STDOUT_UNIT_NUMBER 6
#define GFC_STDERR_UNIT_NUMBER 0

/* F2003 onward. For std < F2003, error caught in array.cc(gfc_match_array_ref).  */
#define GFC_MAX_DIMENSIONS 15

#define GFC_DTYPE_RANK_MASK 0x0F
#define GFC_DTYPE_TYPE_SHIFT 4
#define GFC_DTYPE_TYPE_MASK 0x70
#define GFC_DTYPE_SIZE_SHIFT 7

/* Basic types.  BT_VOID is used by ISO C Binding so funcs like c_f_pointer
   can take any arg with the pointer attribute as a param.  These are also
   used in the run-time library for IO.  */
typedef enum
{ BT_UNKNOWN = 0, BT_INTEGER, BT_LOGICAL, BT_REAL, BT_COMPLEX,
  BT_DERIVED, BT_CHARACTER, BT_CLASS, BT_PROCEDURE, BT_HOLLERITH, BT_VOID,
  BT_ASSUMED, BT_UNION, BT_BOZ, BT_UNSIGNED
}
bt;

/* Enumeration of the possible floating-point types. These values
   correspond to the hidden arguments of the IEEE_CLASS_TYPE
   derived-type of IEEE_ARITHMETIC.  */

enum {
  IEEE_OTHER_VALUE = 0,
  IEEE_SIGNALING_NAN,
  IEEE_QUIET_NAN,
  IEEE_NEGATIVE_INF,
  IEEE_NEGATIVE_NORMAL,
  IEEE_NEGATIVE_DENORMAL,
  IEEE_NEGATIVE_SUBNORMAL = IEEE_NEGATIVE_DENORMAL,
  IEEE_NEGATIVE_ZERO,
  IEEE_POSITIVE_ZERO,
  IEEE_POSITIVE_DENORMAL,
  IEEE_POSITIVE_SUBNORMAL = IEEE_POSITIVE_DENORMAL,
  IEEE_POSITIVE_NORMAL,
  IEEE_POSITIVE_INF
};

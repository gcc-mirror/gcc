/* Machine mode definitions for GNU C-Compiler; included by rtl.h and tree.h.
   Copyright (C) 1991, 1993  Free Software Foundation, Inc.

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
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */


/* Add prototype support.  */
#ifndef PROTO
#if defined (USE_PROTOTYPES) ? USE_PROTOTYPES : defined (__STDC__)
#define PROTO(ARGS) ARGS
#else
#define PROTO(ARGS) ()
#endif
#endif

#ifndef HAVE_MACHINE_MODES

/* Strictly speaking, this isn't the proper place to include these definitions,
   but this file is included by every GCC file.

   Some systems define these in, e.g., param.h.  We undefine these names
   here to avoid the warnings.  We prefer to use our definitions since we
   know they are correct.  */

#undef MIN
#undef MAX

#define MIN(X,Y) ((X) < (Y) ? (X) : (Y))
#define MAX(X,Y) ((X) > (Y) ? (X) : (Y))

/* Find the largest host integer type and set its size and type.  */

#ifndef HOST_BITS_PER_WIDE_INT

#if HOST_BITS_PER_LONG > HOST_BITS_PER_INT
#define HOST_BITS_PER_WIDE_INT HOST_BITS_PER_LONG
#define HOST_WIDE_INT long
#else
#define HOST_BITS_PER_WIDE_INT HOST_BITS_PER_INT
#define HOST_WIDE_INT int
#endif

#endif

/* Provide a default way to print an address in hex via printf.  */

#ifndef HOST_PTR_PRINTF
#define HOST_PTR_PRINTF sizeof (int) == sizeof (char *) ? "%x" : "%lx"
#endif

/* Make an enum class that gives all the machine modes.  */

#define DEF_MACHMODE(SYM, NAME, TYPE, SIZE, UNIT, WIDER)  SYM,

enum machine_mode {
#include "machmode.def"

#ifdef EXTRA_CC_MODES
  EXTRA_CC_MODES,
#endif
MAX_MACHINE_MODE };

#undef DEF_MACHMODE

#define HAVE_MACHINE_MODES

#ifndef NUM_MACHINE_MODES
#define NUM_MACHINE_MODES (int) MAX_MACHINE_MODE
#endif

/* Get the name of mode MODE as a string.  */

extern char *mode_name[];
#define GET_MODE_NAME(MODE)		(mode_name[(int)(MODE)])

enum mode_class { MODE_RANDOM, MODE_INT, MODE_FLOAT, MODE_PARTIAL_INT, MODE_CC,
		  MODE_COMPLEX_INT, MODE_COMPLEX_FLOAT, MAX_MODE_CLASS};

/* Get the general kind of object that mode MODE represents
   (integer, floating, complex, etc.)  */

extern enum mode_class mode_class[];
#define GET_MODE_CLASS(MODE)		(mode_class[(int)(MODE)])

/* Nonzero if MODE is an integral mode.  */
#define INTEGRAL_MODE_P(MODE)			\
  (GET_MODE_CLASS (MODE) == MODE_INT		\
   || GET_MODE_CLASS (MODE) == MODE_PARTIAL_INT \
   || GET_MODE_CLASS (MODE) == MODE_COMPLEX_INT)

/* Nonzero if MODE is a floating-point mode.  */
#define FLOAT_MODE_P(MODE)		\
  (GET_MODE_CLASS (MODE) == MODE_FLOAT	\
   || GET_MODE_CLASS (MODE) == MODE_COMPLEX_FLOAT)

/* Get the size in bytes of an object of mode MODE.  */

extern int mode_size[];
#define GET_MODE_SIZE(MODE)		(mode_size[(int)(MODE)])

/* Get the size in bytes of the basic parts of an object of mode MODE.  */

extern int mode_unit_size[];
#define GET_MODE_UNIT_SIZE(MODE)	(mode_unit_size[(int)(MODE)])

/* Get the number of units in the object.  */

#define GET_MODE_NUNITS(MODE)  \
  ((GET_MODE_UNIT_SIZE ((MODE)) == 0) ? 0 \
   : (GET_MODE_SIZE ((MODE)) / GET_MODE_UNIT_SIZE ((MODE))))

/* Get the size in bits of an object of mode MODE.  */

#define GET_MODE_BITSIZE(MODE)  (BITS_PER_UNIT * mode_size[(int)(MODE)])

/* Get a bitmask containing 1 for all bits in a word
   that fit within mode MODE.  */

#define GET_MODE_MASK(MODE)  \
   ((GET_MODE_BITSIZE (MODE) >= HOST_BITS_PER_WIDE_INT)  \
    ?(HOST_WIDE_INT) ~0 : (((HOST_WIDE_INT) 1 << GET_MODE_BITSIZE (MODE)) - 1))

/* Get the next wider natural mode (eg, QI -> HI -> SI -> DI -> TI).  */

extern enum machine_mode mode_wider_mode[];
#define GET_MODE_WIDER_MODE(MODE)	(mode_wider_mode[(int)(MODE)])

/* Return the mode for data of a given size SIZE and mode class CLASS.
   If LIMIT is nonzero, then don't use modes bigger than MAX_FIXED_MODE_SIZE.
   The value is BLKmode if no other mode is found.  */

extern enum machine_mode mode_for_size PROTO((unsigned int, enum mode_class, int));

/* Find the best mode to use to access a bit field.  */

extern enum machine_mode get_best_mode PROTO((int, int, int, enum machine_mode, int));

/* Determine alignment, 1<=result<=BIGGEST_ALIGNMENT.  */

#define GET_MODE_ALIGNMENT(MODE)   \
  MIN (BIGGEST_ALIGNMENT, 	   \
       MAX (1, (GET_MODE_UNIT_SIZE (MODE) * BITS_PER_UNIT)))

/* For each class, get the narrowest mode in that class.  */

extern enum machine_mode class_narrowest_mode[];
#define GET_CLASS_NARROWEST_MODE(CLASS) class_narrowest_mode[(int)(CLASS)]

/* Define the integer modes whose sizes are BITS_PER_UNIT
   and BITS_PER_WORD.  */

extern enum machine_mode byte_mode;
extern enum machine_mode word_mode;

#endif /* not HAVE_MACHINE_MODES */

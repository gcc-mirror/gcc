/* Machine mode definitions for GNU C-Compiler; included by rtl.h and tree.h.
   Copyright (C) 1991, 93, 94, 96, 98, 99, 2000 Free Software Foundation, Inc.

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

#ifndef HAVE_MACHINE_MODES
#define HAVE_MACHINE_MODES

/* Make an enum class that gives all the machine modes.  */

#define DEF_MACHMODE(SYM, NAME, TYPE, SIZE, UNIT, WIDER)  SYM,

enum machine_mode {
#include "machmode.def"
MAX_MACHINE_MODE };

#undef DEF_MACHMODE

#ifndef NUM_MACHINE_MODES
#define NUM_MACHINE_MODES (int) MAX_MACHINE_MODE
#endif

/* Get the name of mode MODE as a string.  */

extern const char * const mode_name[];
#define GET_MODE_NAME(MODE)		(mode_name[(int) (MODE)])

enum mode_class { MODE_RANDOM, MODE_INT, MODE_FLOAT, MODE_PARTIAL_INT, MODE_CC,
		  MODE_COMPLEX_INT, MODE_COMPLEX_FLOAT, MAX_MODE_CLASS};

/* Get the general kind of object that mode MODE represents
   (integer, floating, complex, etc.)  */

extern const enum mode_class mode_class[];
#define GET_MODE_CLASS(MODE)		(mode_class[(int) (MODE)])

/* Nonzero if MODE is an integral mode.  */
#define INTEGRAL_MODE_P(MODE)			\
  (GET_MODE_CLASS (MODE) == MODE_INT		\
   || GET_MODE_CLASS (MODE) == MODE_PARTIAL_INT \
   || GET_MODE_CLASS (MODE) == MODE_COMPLEX_INT)

/* Nonzero if MODE is a floating-point mode.  */
#define FLOAT_MODE_P(MODE)		\
  (GET_MODE_CLASS (MODE) == MODE_FLOAT	\
   || GET_MODE_CLASS (MODE) == MODE_COMPLEX_FLOAT)

/* Nonzero if MODE is a complex mode.  */
#define COMPLEX_MODE_P(MODE)			\
  (GET_MODE_CLASS (MODE) == MODE_COMPLEX_INT	\
   || GET_MODE_CLASS (MODE) == MODE_COMPLEX_FLOAT)

/* Get the size in bytes of an object of mode MODE.  */

extern const int mode_size[];
#define GET_MODE_SIZE(MODE)		(mode_size[(int) (MODE)])

/* Get the size in bytes of the basic parts of an object of mode MODE.  */

extern const int mode_unit_size[];
#define GET_MODE_UNIT_SIZE(MODE)	(mode_unit_size[(int) (MODE)])

/* Get the number of units in the object.  */

#define GET_MODE_NUNITS(MODE)  \
  ((GET_MODE_UNIT_SIZE ((MODE)) == 0) ? 0 \
   : (GET_MODE_SIZE ((MODE)) / GET_MODE_UNIT_SIZE ((MODE))))

/* Get the size in bits of an object of mode MODE.  */

#define GET_MODE_BITSIZE(MODE)  (BITS_PER_UNIT * mode_size[(int) (MODE)])

#ifdef HOST_WIDE_INT

/* Get a bitmask containing 1 for all bits in a word
   that fit within mode MODE.  */

extern const unsigned HOST_WIDE_INT mode_mask_array[];

#define GET_MODE_MASK(MODE) mode_mask_array[(int) (MODE)]

#endif /* HOST_WIDE_INT */

/* Get the next wider natural mode (eg, QI -> HI -> SI -> DI -> TI).  */

extern const unsigned char mode_wider_mode[];
#define GET_MODE_WIDER_MODE(MODE)	((enum machine_mode)mode_wider_mode[(int) (MODE)])

/* Return the mode for data of a given size SIZE and mode class CLASS.
   If LIMIT is nonzero, then don't use modes bigger than MAX_FIXED_MODE_SIZE.
   The value is BLKmode if no other mode is found.  */

extern enum machine_mode mode_for_size PARAMS ((unsigned int, enum mode_class, int));

/* Similar, but find the smallest mode for a given width.  */

extern enum machine_mode smallest_mode_for_size  PARAMS ((unsigned int,
                                                        enum mode_class));


/* Return an integer mode of the exact same size as the input mode,
   or BLKmode on failure.  */

extern enum machine_mode int_mode_for_mode PARAMS ((enum machine_mode));

/* Find the best mode to use to access a bit field.  */

extern enum machine_mode get_best_mode PARAMS ((int, int, unsigned int,
						enum machine_mode, int));

/* Determine alignment, 1<=result<=BIGGEST_ALIGNMENT.  */

extern unsigned get_mode_alignment PARAMS ((enum machine_mode));

#define GET_MODE_ALIGNMENT(MODE) get_mode_alignment (MODE)

/* For each class, get the narrowest mode in that class.  */

extern const enum machine_mode class_narrowest_mode[];
#define GET_CLASS_NARROWEST_MODE(CLASS) class_narrowest_mode[(int) (CLASS)]

/* Define the integer modes whose sizes are BITS_PER_UNIT and BITS_PER_WORD
   and the mode whose class is Pmode and whose size is POINTER_SIZE.  */

extern enum machine_mode byte_mode;
extern enum machine_mode word_mode;
extern enum machine_mode ptr_mode;

#endif /* not HAVE_MACHINE_MODES */

/* Configuration for GNU C-compiler for Mitsubishi D30V.
   Copyright (C) 1997, 2000 Free Software Foundation, Inc.
   Contributed by Cygnus Solutions.

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

/* #defines that need visibility everywhere.  */
#define	FALSE	0
#define	TRUE	1

/* Define this macro if the host system is System V.  */
/* #define USG */

/* Define this macro if the host system is VMS.  */
/* #define VMS */

/* A C expression for the status code to be returned when the compiler exits
   after serious errors.  */
#define FATAL_EXIT_CODE 33

/* A C expression for the status code to be returned when the compiler exits
   without serious errors.  */
#define SUCCESS_EXIT_CODE 0

/* Defined if the host machine stores words of multi-word values in big-endian
   order.  (GNU CC does not depend on the host byte ordering within a word.)  */
#define HOST_WORDS_BIG_ENDIAN

/* Define this macro to be 1 if the host machine stores `DFmode', `XFmode' or
   `TFmode' floating point numbers in memory with the word containing the sign
   bit at the lowest address; otherwise, define it to be zero.

   This macro need not be defined if the ordering is the same as for multi-word
   integers.  */
/* #define HOST_FLOAT_WORDS_BIG_ENDIAN */

/* A numeric code distinguishing the floating point format for the host
   machine.  See `TARGET_FLOAT_FORMAT' in *Note Storage Layout:: for the
   alternatives and default.  */
/* #define HOST_FLOAT_FORMAT */

/* A C expression for the number of bits in `char' on the host machine.  */
#define HOST_BITS_PER_CHAR 8

/* A C expression for the number of bits in `short' on the host machine.  */
#define HOST_BITS_PER_SHORT 16

/* A C expression for the number of bits in `int' on the host machine.  */
#define HOST_BITS_PER_INT 32

/* A C expression for the number of bits in `long' on the host machine.  */
#define HOST_BITS_PER_LONG 32

/* Define this macro to indicate that the host compiler only supports `int' bit
   fields, rather than other integral types, including `enum', as do most C
   compilers.  */
/* #define ONLY_INT_FIELDS */

/* A C expression for the size of ordinary obstack chunks.  If you don't define
   this, a usually-reasonable default is used.  */
/* #define OBSTACK_CHUNK_SIZE */

/* The function used to allocate obstack chunks.  If you don't define this,
   `xmalloc' is used.  */
/* #define OBSTACK_CHUNK_ALLOC */

/* The function used to free obstack chunks.  If you don't define this, `free'
   is used.  */
/* #define OBSTACK_CHUNK_FREE */

/* Define this macro to indicate that the compiler is running with the `alloca'
   implemented in C.  This version of `alloca' can be found in the file
   `alloca.c'; to use it, you must also alter the `Makefile' variable `ALLOCA'.
   (This is done automatically for the systems on which we know it is needed.)

   If you do define this macro, you should probably do it as follows:

        #ifndef __GNUC__
        #define USE_C_ALLOCA
        #else
        #define alloca __builtin_alloca
        #endif

   so that when the compiler is compiled with GNU CC it uses the more efficient
   built-in `alloca' function.  */

#ifndef __GNUC__
#define USE_C_ALLOCA
#else
#define alloca __builtin_alloca
#endif

/* Define this macro to indicate that the host compiler does not properly
   handle converting a function value to a pointer-to-function when it is used
   in an expression.  */
/* #define FUNCTION_CONVERSION_BUG */

/* Define this if the library function `vprintf' is available on your system.  */
/* #define HAVE_VPRINTF */

/* Define this macro to enable support for multibyte characters in the input to
   GNU CC.  This requires that the host system support the ANSI C library
   functions for converting multibyte characters to wide characters.  */
/* #define MULTIBYTE_CHARS */

/* Define this if the library function `putenv' is available on your system.  */
/* #define HAVE_PUTENV */

/* Define this if your system is POSIX.1 compliant.  */
/* #define POSIX */

/* Define this if your system *does not* provide the variable `sys_siglist'.  */
/* #define NO_SYS_SIGLIST */

/* Define this if your system has the variable `sys_siglist', and there is
   already a declaration of it in the system header files.  */
/* #define DONT_DECLARE_SYS_SIGLIST */

/* Define this to be 1 if you know that the host compiler supports prototypes,
   even if it doesn't define __STDC__, or define it to be 0 if you do not want
   any prototypes used in compiling GNU CC.  If `USE_PROTOTYPES' is not
   defined, it will be determined automatically whether your compiler supports
   prototypes by checking if `__STDC__' is defined.  */
/* #define USE_PROTOTYPES */

/* Define this if you wish suppression of prototypes generated from the machine
   description file, but to use other prototypes within GNU CC.  If
   `USE_PROTOTYPES' is defined to be 0, or the host compiler does not support
   prototypes, this macro has no effect.  */
/* #define NO_MD_PROTOTYPES */

/* Define this if you wish to generate prototypes for the `gen_call' or
   `gen_call_value' functions generated from the machine description file.  If
   `USE_PROTOTYPES' is defined to be 0, or the host compiler does not support
   prototypes, or `NO_MD_PROTOTYPES' is defined, this macro has no effect.  As
   soon as all of the machine descriptions are modified to have the appropriate
   number of arguments, this macro will be removed.

   Some systems do provide this variable, but with a different name such as
   `_sys_siglist'.  On these systems, you can define `sys_siglist' as a macro
   which expands into the name actually provided.  */
/* #define MD_CALL_PROTOTYPES */

/* Define this macro to be a C character constant representing the character
   used to separate components in paths.  The default value is.  the colon
   character */
/* #define PATH_SEPARATOR */

/* If your system uses some character other than slash to separate directory
   names within a file specification, define this macro to be a C character
   constant specifying that character.  When GNU CC displays file names, the
   character you specify will be used.  GNU CC will test for both slash and the
   character you specify when parsing filenames.  */
/* #define DIR_SEPARATOR */

/* Define this macro to be a C string representing the suffix for object files
   on your machine.  If you do not define this macro, GNU CC will use `.o' as
   the suffix for object files.  */
/* #define OBJECT_SUFFIX */

/* Define this macro to be a C string representing the suffix for executable
   files on your machine.  If you do not define this macro, GNU CC will use the
   null string as the suffix for object files.  */
/* #define EXECUTABLE_SUFFIX */

/* If defined, `collect2' will scan the individual object files specified on
   its command line and create an export list for the linker.  Define this
   macro for systems like AIX, where the linker discards object files that are
   not referenced from `main' and uses export lists.  */
/* #define COLLECT_EXPORT_LIST */

/* In addition, configuration files for system V define `bcopy', `bzero' and
   `bcmp' as aliases.  Some files define `alloca' as a macro when compiled with
   GNU CC, in order to take advantage of the benefit of GNU CC's built-in
   `alloca'.  */

/* target machine dependencies.
   tm.h is a symbolic link to the actual target specific file.   */
#include "tm.h"

/* end of xm-generic.h */

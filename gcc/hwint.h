/* HOST_WIDE_INT definitions for the GNU compiler.
   Copyright (C) 1998 Free Software Foundation, Inc.

   This file is part of GNU CC.

   Provide definitions for macros which depend on HOST_BITS_PER_INT
   and HOST_BITS_PER_LONG. */

#ifndef __HWINT_H__
#define __HWINT_H__

/* This describes the machine the compiler is hosted on.  The defaults
   we provide describe the standard 32 bit host since that is the most
   common type supported in gcc.  */
#ifndef HOST_BITS_PER_CHAR
# define HOST_BITS_PER_CHAR 8
#endif
#ifndef HOST_BITS_PER_SHORT
# define HOST_BITS_PER_SHORT 16
#endif
#ifndef HOST_BITS_PER_INT
# define HOST_BITS_PER_INT 32
#endif
#ifndef HOST_BITS_PER_LONG
# define HOST_BITS_PER_LONG 32
#endif
#ifndef HOST_BITS_PER_LONGLONG
# define HOST_BITS_PER_LONGLONG 64
#endif

/* Find the largest host integer type and set its size and type.  */

#ifndef HOST_BITS_PER_WIDE_INT

# if HOST_BITS_PER_LONG > HOST_BITS_PER_INT
#  define HOST_BITS_PER_WIDE_INT HOST_BITS_PER_LONG
#  define HOST_WIDE_INT long
# else
#  define HOST_BITS_PER_WIDE_INT HOST_BITS_PER_INT
#  define HOST_WIDE_INT int
# endif

#endif /* ! HOST_BITS_PER_WIDE_INT */


/* Provide defaults for the way to print a HOST_WIDE_INT
   in various manners.  */

#ifndef HOST_WIDE_INT_PRINT_DEC
# if HOST_BITS_PER_WIDE_INT == HOST_BITS_PER_INT
#  define HOST_WIDE_INT_PRINT_DEC "%d"
# else
#  if HOST_BITS_PER_WIDE_INT == HOST_BITS_PER_LONG
#   define HOST_WIDE_INT_PRINT_DEC "%ld"
#  else
#   define HOST_WIDE_INT_PRINT_DEC "%lld"
#  endif
# endif
#endif /* ! HOST_WIDE_INT_PRINT_DEC */

#ifndef HOST_WIDE_INT_PRINT_UNSIGNED
# if HOST_BITS_PER_WIDE_INT == HOST_BITS_PER_INT
#  define HOST_WIDE_INT_PRINT_UNSIGNED "%u"
# else
#  if HOST_BITS_PER_WIDE_INT == HOST_BITS_PER_LONG
#   define HOST_WIDE_INT_PRINT_UNSIGNED "%lu"
#  else
#   define HOST_WIDE_INT_PRINT_UNSIGNED "%llu"
#  endif
# endif
#endif /* ! HOST_WIDE_INT_PRINT_UNSIGNED */

#ifndef HOST_WIDE_INT_PRINT_HEX
# if HOST_BITS_PER_WIDE_INT == HOST_BITS_PER_INT
#  define HOST_WIDE_INT_PRINT_HEX "0x%x"
# else
#  if HOST_BITS_PER_WIDE_INT == HOST_BITS_PER_LONG
#   define HOST_WIDE_INT_PRINT_HEX "0x%lx"
#  else
#   define HOST_WIDE_INT_PRINT_HEX "0x%llx"
#  endif
# endif
#endif /* ! HOST_WIDE_INT_PRINT_HEX */

#ifndef HOST_WIDE_INT_PRINT_DOUBLE_HEX
# if HOST_BITS_PER_WIDE_INT == 64
#  if HOST_BITS_PER_WIDE_INT == HOST_BITS_PER_INT
#   define HOST_WIDE_INT_PRINT_DOUBLE_HEX "0x%x%016x"
#  else
#   if HOST_BITS_PER_WIDE_INT == HOST_BITS_PER_LONG
#    define HOST_WIDE_INT_PRINT_DOUBLE_HEX "0x%lx%016lx"
#   else
#    define HOST_WIDE_INT_PRINT_DOUBLE_HEX "0x%llx%016llx"
#   endif
#  endif
# else
#  if HOST_BITS_PER_WIDE_INT == HOST_BITS_PER_INT
#   define HOST_WIDE_INT_PRINT_DOUBLE_HEX "0x%x%08x"
#  else
#   if HOST_BITS_PER_WIDE_INT == HOST_BITS_PER_LONG
#    define HOST_WIDE_INT_PRINT_DOUBLE_HEX "0x%lx%08lx"
#   else
#    define HOST_WIDE_INT_PRINT_DOUBLE_HEX "0x%llx%08llx"
#   endif
#  endif
# endif
#endif /* ! HOST_WIDE_INT_PRINT_DOUBLE_HEX */

#endif /* __HWINT_H__ */

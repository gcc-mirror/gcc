

#if defined( SYSTYPES_STDLIB_SIZE_T_CHECK )
#ifndef __SIZE_TYPE__
#define __SIZE_TYPE__ long unsigned int
#endif
#ifndef _GCC_SIZE_T
#define _GCC_SIZE_T
typedef __SIZE_TYPE__ size_t; /* size of something */
#endif
#endif  /* SYSTYPES_STDLIB_SIZE_T_CHECK */



#if defined( IRIX_MULTILINE_CMNT_CHECK )
/* we check the type of the result */
// /* of the sizeof something.  This is a bad test :-( */
#endif  /* IRIX_MULTILINE_CMNT_CHECK */


#if defined( SYSTYPES_STDLIB_SIZE_T_CHECK )
#ifndef __SIZE_TYPE__
#define __SIZE_TYPE__ long unsigned int
#endif
#ifndef _GCC_SIZE_T
#define _GCC_SIZE_T
typedef __SIZE_TYPE__ size_t; /* size of something */
#endif
#endif  /* SYSTYPES_STDLIB_SIZE_T_CHECK */

#ifndef FIXINC_BROKEN_ASSERT_STDLIB_CHECK
#define FIXINC_BROKEN_ASSERT_STDLIB_CHECK 1

#ifdef __cplusplus
#include <stdlib.h>
#endif
#ifndef FIXINC_BROKEN_ASSERT_STDIO_CHECK
#define FIXINC_BROKEN_ASSERT_STDIO_CHECK 1

#include <stdio.h>


#if defined( BROKEN_ASSERT_STDIO_CHECK )
extern FILE* stderr;
#endif  /* BROKEN_ASSERT_STDIO_CHECK */


#if defined( BROKEN_ASSERT_STDLIB_CHECK )
extern void exit ( int );
#endif  /* BROKEN_ASSERT_STDLIB_CHECK */

#endif  /* FIXINC_BROKEN_ASSERT_STDIO_CHECK */

#endif  /* FIXINC_BROKEN_ASSERT_STDLIB_CHECK */

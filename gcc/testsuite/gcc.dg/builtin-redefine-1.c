/* Test default warnings for redefining builtin macros.  */

/* { dg-do compile } */
/* { dg-options "-D__TIMESTAMP__=x -D__TIME__=x -D__DATE__=x -D__FILE__=x -D__FILE_NAME__=x -D__BASE_FILE__=x -D__LINE__=0" } */

/* Check default behavior for builtin macros redefinition.  */

/* { dg-message "\"__TIMESTAMP__\" redefined" "" {target "*-*-*"} 0 } */
#ifndef __TIMESTAMP__
#error "__TIMESTAMP__ builtin is not defined"
/* { dg-bogus "Expected built-in is not defined" "" { target *-*-* } .-1 } */
#endif

/* { dg-message "\"__TIME__\" redefined" "" {target "*-*-*"} 0 } */
#ifndef __TIME__
#error "__TIME__ builtin is not defined"
/* { dg-bogus "Expected built-in is not defined" "" { target *-*-* } .-1 } */
#endif

/* { dg-message "\"__DATE__\" redefined" "" {target "*-*-*"} 0 } */
#ifndef __DATE__
#error "__DATE__ builtin is not defined"
/* { dg-bogus "Expected built-in is not defined" "" { target *-*-* } .-1 } */
#endif

/* { dg-message "\"__FILE__\" redefined" "" {target "*-*-*"} 0 } */
#ifndef __FILE__
#error "__FILE__ builtin is not defined"
/* { dg-bogus "Expected built-in is not defined" "" { target *-*-* } .-1 } */
#endif

/* { dg-message "\"__FILE_NAME__\" redefined" "" {target "*-*-*"} 0 } */
#ifndef __FILE_NAME__
#error "__FILE_NAME__ builtin is not defined"
/* { dg-bogus "Expected built-in is not defined" "" { target *-*-* } .-1 } */
#endif

/* { dg-message "\"__BASE_FILE__\" redefined" "" {target "*-*-*"} 0 } */
#ifndef __BASE_FILE__
#error "__BASE_FILE__ builtin is not defined"
/* { dg-bogus "Expected built-in is not defined" "" { target *-*-* } .-1 } */
#endif

/* { dg-message "\"__LINE__\" redefined" "" {target "*-*-*"} 0 } */
#ifndef __LINE__
#error "__LINE__ builtin is not defined"
/* { dg-bogus "Expected built-in is not defined" "" { target *-*-* } .-1 } */
#endif


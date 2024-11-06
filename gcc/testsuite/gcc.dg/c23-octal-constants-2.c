/* Test that octal constants are diagnosed in C23 mode: -pedantic-errors.  */
/* { dg-do compile } */
/* { dg-options "-std=c23 -pedantic-errors" } */

int a = 0o7; /* { dg-error "'0o' prefixed constants" } */
#if 0o107 /* { dg-error "'0o' prefixed constants" } */
#endif

int b = 0O7; /* { dg-error "'0o' prefixed constants" } */
#if 0O107 /* { dg-error "'0o' prefixed constants" } */
#endif

/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */
/* Check that 'noreturn' and 'volatile extern' are compatible.  
   The testsuite uses -ansi -pedantic-errors by default, so this has
   to override.  */
extern void xxx (int) __attribute__((noreturn));
__volatile extern void xxx (int);

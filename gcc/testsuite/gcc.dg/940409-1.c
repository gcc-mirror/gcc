/* GCC should allow struct S to be in a register.  */
/* { dg-do compile } */

struct S { volatile int field; };
int f (register struct S arg);  /* { dg-bogus "volatile field" "with arg" } */
int g (register struct S);	/* { dg-bogus "volatile field" "no arg" } */

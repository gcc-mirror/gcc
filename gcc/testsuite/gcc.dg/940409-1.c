/* GCC should allow struct S to be in a register, but it doesn't.  This is
   an obscure corner case, hasn't worked since 1994, and we don't expect it
   to work anytime soon, hence XFAIL.  */
/* { dg-do compile } */

struct S { volatile int field; };
int f (register struct S arg);  /* { dg-bogus "volatile field" "with arg" { xfail *-*-* } } */
int g (register struct S);	/* { dg-bogus "volatile field" "no arg" { xfail *-*-* } } */

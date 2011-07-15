/* { dg-do compile } */
/* { dg-options "-O2 -mlong-calls" } */
/* { dg-final { scan-assembler-times "\\tcall\[p\]*\[\\t ]*.s" 3 } } */
/* { dg-final { scan-assembler "call\[p\]*\[\\t ]*.s.\[\\t ]*.f" } } */
/* { dg-final { scan-assembler-not "call\[p\]*\[\\t ]*.s.\[\\t ]*.g" } } */
/* { dg-final { scan-assembler-not "call\[p\]*\[\\t ]*.s.\[\\t ]*.h" } } */

int x;

static __attribute__ ((noinline)) void f ()
{
  x = 5;
}

extern void g ();

static __attribute__ ((noinline)) __attribute__((section(".init.text"))) void h ()
{
  x = 5;
}

int bar ()
{
  f ();
  g ();
  h ();
}

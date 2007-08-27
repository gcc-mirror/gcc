/* { dg-do compile } */
/* { dg-require-effective-target ilp32 } */
/* { dg-require-effective-target nonpic } */
/* { dg-options "-O2 -funit-at-a-time -fomit-frame-pointer" } */
/* { dg-final { scan-assembler-not "sub\[^\\n\]*sp" } } */

static __attribute__ ((noinline)) q ();
int a;

/* This function should not require any stack manipulation
   for preferred stack bounday.  */
void
e ()
{
  if (a)
  {
    e ();
    a--;
  }
  q ();
}

static __attribute__ ((noinline)) q ()
{
}

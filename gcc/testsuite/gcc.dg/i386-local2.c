/* { dg-do compile { target i?86-*-* x86_64-*-* } } */
/* { dg-options "-O2 -funit-at-a-time -fomit-frame-pointer" } */
/* { dg-skip-if "PR 25214" { ilp32 } { "-fpic" "-fPIC" } { "" } } */
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

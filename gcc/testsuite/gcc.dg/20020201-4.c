/* This testcase failed because recog_for_combine used to pass a different
   pattern than contained in insn to recog.  */
/* { dg-do compile } */
/* { dg-options "-O2 -fssa -fssa-ccp" } */
/* { dg-options "-O2 -march=i686 -fssa -fssa-ccp" { target i?86-*-* } } */

extern int bar (char *);

int
foo (void)
{
  char b[512];

  bar (b);
  return __builtin_strlen (b);
}

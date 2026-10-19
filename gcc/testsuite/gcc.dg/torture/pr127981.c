/* PR middle-end/127981 */
/* { dg-do run } */
/* { dg-additional-options "-fgimple" } */

/* The address of x only becomes live in bb 3 through the PHI while y is
   already live coming in from bb 2.  Make sure x and y are recorded as
   conflicting and do not share a stack slot.  */

int **p;
int g;
int a[3];

short __GIMPLE (ssa,startwith("expand")) __attribute__((noipa))
foo (int c)
{
  short x;
  int *y;
  short *q;
  short _1;
  int **_2;

  __BB(2):
  p = &y;
  if (c_3(D) != 0)
    goto __BB3;
  else
    goto __BB4;

  __BB(3):
  q_4 = __PHI (__BB2: &x);
  y = &g;
  x = _Literal (short) 0;
  _2 = p;
  *_2 = &a[2];
  _1 = *q_4;
  x = __CLOBBER(eos);
  y = __CLOBBER(eos);
  return _1;

  __BB(4):
  return _Literal (short) 1;
}

int
main ()
{
  if (foo (1) != 0)
    __builtin_abort ();
  return 0;
}

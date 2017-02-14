/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-pre" } */

unsigned short f(unsigned short a)
{
  if (a & 0x8000)
    a <<= 1, a = a ^ 0x1021;
  else
    a <<= 1;

  return a;
}

/* We should hoist and CSE the shift.  */

/* { dg-final { scan-tree-dump-times " << 1;" 1 "pre" } } */

/* { dg-do compile } */
/* { dg-options "-O -m8bit-idiv -fira-algorithm=priority" } */
/* This is the same as divmod-5.c, just with different options which
   trigger an ICE.  We don't look at the output.  */

extern void foo (int, int, int, int, int, int);

void
bar (int x, int y)
{
  foo (0, 0, 0, 0, x / y, x % y);
}


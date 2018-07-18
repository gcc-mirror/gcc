/* { dg-do compile } */
/* { dg-require-effective-target nonlocal_goto } */

long buf[5];
int bar (void);

int
foo (int x)
{
  int y = __builtin_setjmp (buf);
  while (x != 3 && x && x && x != 2)
    x = bar ();
  return y;
}

/* Verify that rtl expansion cleanup doesn't get too aggressive about
   code dealing with complex CONCATs.  */
/* { dg-do run } */
/* { dg-options "-O -fno-tree-sra" } */

extern void abort (void);
extern void exit (int);

__complex__ float foo (void)
{
  __complex__ float f[1];
  __real__ f[0] = 1;
  __imag__ f[0] = 1;
  f[0] = __builtin_conjf (f[0]);
  return f[0];
}

int main (void)
{
  __complex__ double d[1];
  d[0] = foo ();
  if (__real__ d[0] != 1 || __imag__ d[0] != -1)
    abort ();
  exit (0);
}

/* PR target/30665: bug in cris.md peephole2 condition.
   Testcase for trunk.  */
/* { dg-do run } */

extern void abort (void);
extern void exit (int);

int  __attribute__ ((__noinline__)) f (unsigned *p, int *x)
{
  int y = *p++ & 0xfff;
  *x++ = y;
  *x = *p;
  return y;
}

int main (void)
{
  unsigned u[2] = { 0x3aad, 0x5ad1 };
  int x[2] = {17689, 23456};

  if (f (u, x) != 0xaad || x[0] != 0xaad || x[1] != 0x5ad1)
    abort ();
  exit (0);
}

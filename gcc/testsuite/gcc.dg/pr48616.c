/* PR tree-optimization/48616 */
/* { dg-do run } */
/* { dg-options "-O2 -ftree-vectorize" } */
/* { dg-additional-options "-fno-common" { target hppa*-*-hpux* } } */

extern void abort (void);
int a[4] __attribute__((aligned (32)));
int b[4] __attribute__((aligned (32)));
int c[4] __attribute__((aligned (32)));
int d[4] __attribute__((aligned (32)));
int e[4] __attribute__((aligned (32)));

__attribute__((noinline, noclone))
int
foo (int x)
{
  asm ("" : "+r" (x));
  return x;
}

__attribute__((noinline, noclone))
void
fn1 (int i)
{
  a[0] = b[0] << c[0];
  a[1] = b[1] << c[1];
  a[2] = b[2] << c[2];
  a[3] = b[3] << c[3];
  if (i)
    {
      d[0] = e[0] >> c[0];
      d[1] = e[1] >> c[1];
      d[2] = e[2] >> c[2];
      d[3] = e[3] >> c[3];
    }
}

__attribute__((noinline, noclone))
void
fn2 (int i)
{
  a[0] = b[0] << 1;
  a[1] = b[1] << 2;
  a[2] = b[2] << 3;
  a[3] = b[3] << 4;
  if (i)
    {
      d[0] = e[0] >> 1;
      d[1] = e[1] >> 2;
      d[2] = e[2] >> 3;
      d[3] = e[3] >> 4;
    }
}

__attribute__((noinline, noclone))
void
fn3 (int i, int j)
{
  int x = foo (j);
  a[0] = b[0] << x;
  a[1] = b[1] << x;
  a[2] = b[2] << x;
  a[3] = b[3] << x;
  if (i)
    {
      d[0] = e[0] >> x;
      d[1] = e[1] >> x;
      d[2] = e[2] >> x;
      d[3] = e[3] >> x;
    }
}

__attribute__((noinline, noclone))
void
fn4 (int i)
{
  a[0] = b[0] << 1;
  a[1] = b[1] << 1;
  a[2] = b[2] << 1;
  a[3] = b[3] << 1;
  if (i)
    {
      d[0] = e[0] >> 1;
      d[1] = e[1] >> 1;
      d[2] = e[2] >> 1;
      d[3] = e[3] >> 1;
    }
}

int
main ()
{
  int i;
  int *t;
  for (i = 0; i < 4; i++)
    {
      b[i] = 32;
      c[i] = i + 1;
      e[i] = 32;
    }
  asm volatile ("" : : "r" (b) : "memory");
  asm volatile ("" : : "r" (c) : "memory");
  asm volatile ("" : "=r" (t) : "0" (d) : "memory");
  fn1 (t != 0);
  for (i = 0; i < 4; i++)
    {
      if (a[i] != (32 << (i + 1)) || d[i] != (32 >> (i + 1)))
	abort ();
      a[i] = 0;
      d[i] = 0;
    }
  fn2 (t != 0);
  for (i = 0; i < 4; i++)
    {
      if (a[i] != (32 << (i + 1)) || d[i] != (32 >> (i + 1)))
	abort ();
      a[i] = 0;
      d[i] = 0;
    }
  fn3 (t != 0, t != 0);
  for (i = 0; i < 4; i++)
    {
      if (a[i] != (32 << 1) || d[i] != (32 >> 1))
	abort ();
      a[i] = 0;
      d[i] = 0;
    }
  fn4 (t != 0);
  for (i = 0; i < 4; i++)
    {
      if (a[i] != (32 << 1) || d[i] != (32 >> 1))
	abort ();
    }
  return 0;
}

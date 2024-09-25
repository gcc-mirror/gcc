/* PR target/20322 */
/* { dg-do run } */

extern void abort (void);

typedef unsigned long T;
typedef struct
{
  T a, b;
  unsigned char c, d;
} S;

#define M (sizeof (T) * 4)

S __attribute__((noinline))
foo (T x, T y)
{
  S e;
  T f[2], g;

  e.b = (x & (~(T) 0 >> M)) * (y & (~(T) 0 >> M));
  e.a = (x >> M) * (y >> M);

  f[0] = (x & (~(T) 0 >> M)) * (y >> M);
  f[1] = (x >> M) * (y & (~(T) 0 >> M));

  g = e.b;
  e.b += (f[0] & (~(T) 0 >> M)) << M;
  if (e.b < g)
    e.a++;

  g = e.b;
  e.b += (f[1] & (~(T) 0 >> M)) << M;
  if (e.b < g)
    e.a++;

  e.a += (f[0] >> M);
  e.a += (f[1] >> M);
  e.c = 1;
  e.d = 0;

  return e;
}

int
main (void)
{
  T x = 1UL << (M * 2 - 1);
  S y = foo (1, x);
  if (y.a || y.b != x || y.c != 1 || y.d)
    abort ();
  return 0;
}

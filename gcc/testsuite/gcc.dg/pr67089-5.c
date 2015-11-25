/* PR target/67089 */
/* { dg-do run } */
/* { dg-options "-O2 -ftree-loop-if-convert" } */

extern void abort (void);

int cnt;
unsigned int a[16], b[16], c[16];

__attribute__((noinline, noclone))
void foo (int x)
{
  asm volatile ("" : : "g" (x) : "memory");
  cnt++;
}

__attribute__((noinline, noclone)) void
f0 (unsigned int x)
{
  for (int i = 0; i < 16; i++)
    {
      unsigned int r = x - a[i];
      b[i] = r;
      c[i] = r > x ? 7 : x;
    }
}

#define T(n, type, op, cond) \
__attribute__((noinline, noclone))	\
type					\
f##n (type x)				\
{					\
  type r = op;				\
  cond;					\
  return r;				\
}

T (1, unsigned int, x - 2U, if (r > x) foo (0))
T (2, unsigned long, x - 2U, if (r <= x) foo (0))
T (3, unsigned short, 2U - x, if (r > 2U) foo (0))
T (4, unsigned char, 2U - x, if (r <= 2U) foo (0))
T (5, unsigned int, x + -2U, if (r > x) foo (0))
T (6, unsigned long, x + -2UL, if (r <= x) foo (0))
T (7, unsigned short, (unsigned short) -2 + x, if (r > (unsigned short) -2) foo (0))
T (8, unsigned char, (unsigned char) -2 + x, if (r <= (unsigned char) -2) foo (0))

int
main ()
{
  int i;
  for (i = 0; i < 16; i++)
    a[i] = i - 7;
  f0 (5);
  for (i = 0; i < 16; i++)
    if (b[i] != 12U - i || c[i] != 7 - 2 * (i >= 7 && i < 13))
      abort ();
  if (f1 (3) != 1 || cnt != 0) abort ();
  if (f1 (2) != 0 || cnt != 0) abort ();
  if (f1 (1) != -1U || cnt != 1) abort ();
  if (f2 (3) != 1 || cnt != 2) abort ();
  if (f2 (2) != 0 || cnt != 3) abort ();
  if (f2 (1) != -1UL || cnt != 3) abort ();
  if (f3 (3) != (unsigned short) -1 || cnt != 4) abort ();
  if (f3 (2) != 0 || cnt != 4) abort ();
  if (f3 (1) != 1 || cnt != 4) abort ();
  if (f4 (3) != (unsigned char) -1 || cnt != 4) abort ();
  if (f4 (2) != 0 || cnt != 5) abort ();
  if (f4 (1) != 1 || cnt != 6) abort ();
  if (f5 (3) != 1 || cnt != 6) abort ();
  if (f5 (2) != 0 || cnt != 6) abort ();
  if (f5 (1) != -1U || cnt != 7) abort ();
  if (f6 (3) != 1 || cnt != 8) abort ();
  if (f6 (2) != 0 || cnt != 9) abort ();
  if (f6 (1) != -1UL || cnt != 9) abort ();
  if (f7 (3) != 1 || cnt != 9) abort ();
  if (f7 (2) != 0 || cnt != 9) abort ();
  if (f7 (1) != (unsigned short) -1 || cnt != 10) abort ();
  if (f8 (3) != 1 || cnt != 11) abort ();
  if (f8 (2) != 0 || cnt != 12) abort ();
  if (f8 (1) != (unsigned char) -1 || cnt != 12) abort ();
  return 0;
}

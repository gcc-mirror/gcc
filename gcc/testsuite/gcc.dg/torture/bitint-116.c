/* PR tree-optimization/121722 */
/* { dg-do run { target bitint575 } } */

typedef unsigned _BitInt(129) U129;
typedef _BitInt(575) S575;
typedef unsigned _BitInt(575) U575;

/* A swap through the loop PHIs.  */

[[gnu::noipa]] U129
fib (unsigned long n)
{
  U129 prev = 0, cur = 1;
  for (unsigned long i = 2; i <= n; ++i)
    {
      U129 next = prev + cur;
      prev = cur;
      cur = next;
    }
  return cur;
}

/* Copies that cross on one edge into a join.  */

[[gnu::noipa]] S575
cross (S575 a, S575 b, S575 p, S575 q)
{
  S575 x = a < b ? p : q;
  S575 z = a < b ? q : a;
  return x + z;
}

/* A constant store on the latch edge and a copy that reads the old
   value.  */

#define CST(name, K) \
[[gnu::noipa]] U575 \
name (int n, U575 a0) \
{ \
  U575 a = a0, b = 7, s = 0; \
  for (int i = 0; i < n; ++i) \
    { \
      s += a + b; \
      b = a; \
      a = K; \
    } \
  return s; \
}

#define W (~(U575) 0 / 3)

CST (cst0, 0)
CST (cst5, 5)
CST (cstm1, -1)
CST (cstw, W)

/* Rotations in both directions.  */

[[gnu::noipa]] U575
rot (int n)
{
  U575 a = 1, b = 2, c = 3, s = 0;
  for (int i = 0; i < n; ++i)
    {
      s = s * 10 + a;
      U575 t = a;
      a = b;
      b = c;
      c = t;
    }
  return s;
}

[[gnu::noipa]] U575
rotr (int n)
{
  U575 a = 1, b = 2, c = 3, s = 0;
  for (int i = 0; i < n; ++i)
    {
      s = s * 10 + a;
      U575 t = c;
      c = b;
      b = a;
      a = t;
    }
  return s;
}

/* Two copies that read the old value of one partition.  */

[[gnu::noipa]] U575
share (int n)
{
  U575 a = 1, b = 0, c = 5, s = 0;
  for (int i = 0; i < n; ++i)
    {
      s = s * 10 + a + b + c;
      U575 t = a;
      a = a + 1;
      b = t;
      c = t;
    }
  return s;
}

/* A swap of the parameters.  */

[[gnu::noipa]] S575
pswap (S575 a, S575 b, int n)
{
  for (int i = 0; i < n; ++i)
    {
      S575 t = a;
      a = b;
      b = t;
    }
  return a * 3 + b;
}

int
main ()
{
  if (fib (10) != 55)
    __builtin_abort ();
  if (cross (1, 2, 3, 4) != 7 || cross (2, 1, 3, 4) != 6)
    __builtin_abort ();
  if (cst0 (2, 5) != 17
      || cst5 (2, 5) != 22
      || cstm1 (2, 5) != 16
      || cstw (2, 5) != 17 + W)
    __builtin_abort ();
  if (rot (4) != 1231 || rotr (4) != 1321)
    __builtin_abort ();
  if (share (3) != 647)
    __builtin_abort ();
  if (pswap (1, 2, 3) != 7 || pswap (1, 2, 2) != 5)
    __builtin_abort ();
}

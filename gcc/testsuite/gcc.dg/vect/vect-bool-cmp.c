/* PR tree-optimization/71488 */
/* { dg-require-effective-target vect_int } */
/* { dg-require-effective-target vect_pack_trunc } */
/* { dg-additional-options "-msse4" { target sse4_runtime } } */

#include "tree-vect.h"

int i1, i2;

void __attribute__((noclone,noinline))
fn1 (int * __restrict__ p1, int * __restrict__ p2, int * __restrict__ p3, int size)
{
  int i;

  for (i = 0; i < size; i++)
    p1[i] = ((p2[i] == 0) > (unsigned)(p3[i] == 0)) + (p2[i] == 0);
}

void __attribute__((noclone,noinline))
fn2 (int * __restrict__ p1, int * __restrict__ p2, short * __restrict__ p3, int size)
{
  int i;

  for (i = 0; i < size; i++)
    p1[i] = ((p2[i] == 0) > (unsigned)(p3[i] == 0)) + (p2[i] == 0);
}

void __attribute__((noclone,noinline))
fn3 (int * __restrict__ p1, int * __restrict__ p2, long long * __restrict__ p3, int size)
{
  int i;

  for (i = 0; i < size; i++)
    p1[i] = ((p2[i] == 0) > (unsigned)(p3[i] == 0)) + (p2[i] == 0);
}

void __attribute__((noclone,noinline))
fn4 (int * __restrict__ p1, int * __restrict__ p2, int * __restrict__ p3, int size)
{
  int i;

  for (i = 0; i < size; i++)
    p1[i] = ((p2[i] == 0) >= (unsigned)(p3[i] == 0)) + (p2[i] == 0);
}

void __attribute__((noclone,noinline))
fn5 (int * __restrict__ p1, int * __restrict__ p2, short * __restrict__ p3, int size)
{
  int i;

  for (i = 0; i < size; i++)
    p1[i] = ((p2[i] == 0) >= (unsigned)(p3[i] == 0)) + (p2[i] == 0);
}

void __attribute__((noclone,noinline))
fn6 (int * __restrict__ p1, int * __restrict__ p2, long long * __restrict__ p3, int size)
{
  int i;

  for (i = 0; i < size; i++)
    p1[i] = ((p2[i] == 0) >= (unsigned)(p3[i] == 0)) + (p2[i] == 0);
}

void __attribute__((noclone,noinline))
fn7 (int * __restrict__ p1, int * __restrict__ p2, int * __restrict__ p3, int size)
{
  int i;

  for (i = 0; i < size; i++)
    p1[i] = ((p2[i] == 0) < (unsigned)(p3[i] == 0)) + (p2[i] == 0);
}

void __attribute__((noclone,noinline))
fn8 (int * __restrict__ p1, int * __restrict__ p2, short * __restrict__ p3, int size)
{
  int i;

  for (i = 0; i < size; i++)
    p1[i] = ((p2[i] == 0) < (unsigned)(p3[i] == 0)) + (p2[i] == 0);
}

void __attribute__((noclone,noinline))
fn9 (int * __restrict__ p1, int * __restrict__ p2, long long * __restrict__ p3, int size)
{
  int i;

  for (i = 0; i < size; i++)
    p1[i] = ((p2[i] == 0) < (unsigned)(p3[i] == 0)) + (p2[i] == 0);
}

void __attribute__((noclone,noinline))
fn10 (int * __restrict__ p1, int * __restrict__ p2, int * __restrict__ p3, int size)
{
  int i;

  for (i = 0; i < size; i++)
    p1[i] = ((p2[i] == 0) <= (unsigned)(p3[i] == 0)) + (p2[i] == 0);
}

void __attribute__((noclone,noinline))
fn11 (int * __restrict__ p1, int * __restrict__ p2, short * __restrict__ p3, int size)
{
  int i;

  for (i = 0; i < size; i++)
    p1[i] = ((p2[i] == 0) <= (unsigned)(p3[i] == 0)) + (p2[i] == 0);
}

void __attribute__((noclone,noinline))
fn12 (int * __restrict__ p1, int * __restrict__ p2, long long * __restrict__ p3, int size)
{
  int i;

  for (i = 0; i < size; i++)
    p1[i] = ((p2[i] == 0) <= (unsigned)(p3[i] == 0)) + (p2[i] == 0);
}

void __attribute__((noclone,noinline))
fn13 (int * __restrict__ p1, int * __restrict__ p2, int * __restrict__ p3, int size)
{
  int i;

  for (i = 0; i < size; i++)
    p1[i] = ((p2[i] == 0) == (unsigned)(p3[i] == 0)) + (p2[i] == 0);
}

void __attribute__((noclone,noinline))
fn14 (int * __restrict__ p1, int * __restrict__ p2, short * __restrict__ p3, int size)
{
  int i;

  for (i = 0; i < size; i++)
    p1[i] = ((p2[i] == 0) == (unsigned)(p3[i] == 0)) + (p2[i] == 0);
}

void __attribute__((noclone,noinline))
fn15 (int * __restrict__ p1, int * __restrict__ p2, long long * __restrict__ p3, int size)
{
  int i;

  for (i = 0; i < size; i++)
    p1[i] = ((p2[i] == 0) == (unsigned)(p3[i] == 0)) + (p2[i] == 0);
}

void __attribute__((noclone,noinline))
fn16 (int * __restrict__ p1, int * __restrict__ p2, int * __restrict__ p3, int size)
{
  int i;

  for (i = 0; i < size; i++)
    p1[i] = ((p2[i] == 0) != (unsigned)(p3[i] == 0)) + (p2[i] == 0);
}

void __attribute__((noclone,noinline))
fn17 (int * __restrict__ p1, int * __restrict__ p2, short * __restrict__ p3, int size)
{
  int i;

  for (i = 0; i < size; i++)
    p1[i] = ((p2[i] == 0) != (unsigned)(p3[i] == 0)) + (p2[i] == 0);
}

void __attribute__((noclone,noinline))
fn18 (int * __restrict__ p1, int * __restrict__ p2, long long * __restrict__ p3, int size)
{
  int i;

  for (i = 0; i < size; i++)
    p1[i] = ((p2[i] == 0) != (unsigned)(p3[i] == 0)) + (p2[i] == 0);
}

int eq (int i1, int i2) { return i1 == i2; }
int ne (int i1, int i2) { return i1 != i2; }
int lt (int i1, int i2) { return i1 < i2; }
int le (int i1, int i2) { return i1 <= i2; }
int gt (int i1, int i2) { return i1 > i2; }
int ge (int i1, int i2) { return i1 >= i2; }

typedef int (*cmp_fn)(int, int);

void
check (int *p, cmp_fn fn)
{
  int i;

#pragma GCC novector
  for (i = 0; i < 32; i++)
    {
      int t1 = ((i % 4) > 1) == 0;
      int t2 = (i % 2) == 0;
      int res = fn (t1, t2) + t1;
      if (p[i] != res)
	__builtin_abort ();
    }
}

int
main (int argc, char **argv)
{
  int i1[32], i2[32], res[32];
  short s2[32];
  long long l2[32];
  int i;

  check_vect ();

  for (i = 0; i < 32; i++)
    {
      l2[i] = i2[i] = s2[i] = i % 2;
      i1[i] = (i % 4) > 1;
      asm ("":::"memory");
    }

  fn1 (res, i1, i2, 32);
  check (res, gt);
  fn2 (res, i1, s2, 32);
  check (res, gt);
  fn3 (res, i1, l2, 32);
  check (res, gt);

  fn4 (res, i1, i2, 32);
  check (res, ge);
  fn5 (res, i1, s2, 32);
  check (res, ge);
  fn6 (res, i1, l2, 32);
  check (res, ge);

  fn7 (res, i1, i2, 32);
  check (res, lt);
  fn8 (res, i1, s2, 32);
  check (res, lt);
  fn9 (res, i1, l2, 32);
  check (res, lt);

  fn10 (res, i1, i2, 32);
  check (res, le);
  fn11 (res, i1, s2, 32);
  check (res, le);
  fn12 (res, i1, l2, 32);
  check (res, le);

  fn13 (res, i1, i2, 32);
  check (res, eq);
  fn14 (res, i1, s2, 32);
  check (res, eq);
  fn15 (res, i1, l2, 32);
  check (res, eq);

  fn16 (res, i1, i2, 32);
  check (res, ne);
  fn17 (res, i1, s2, 32);
  check (res, ne);
  fn18 (res, i1, l2, 32);
  check (res, ne);
}

/* { dg-final { scan-tree-dump-times "LOOP VECTORIZED" 18 "vect" { target sse4_runtime } } } */

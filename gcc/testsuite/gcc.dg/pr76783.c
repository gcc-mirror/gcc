/* { dg-do run } */
/* { dg-require-effective-target int128 } */
/* { dg-options "-Og -w -Wno-psabi" } */

typedef unsigned __int128 u128;
typedef unsigned __int128 v64u128 __attribute__ ((vector_size (64)));

u128 __attribute__ ((noinline, noclone))
foo (unsigned c, v64u128 v)
{
  v64u128 u;
  if (c) {
    u = (v64u128){(u128)0, (u128)0};
  } else {
    u = (v64u128){(u128)0, (u128)1};
  }
  u += v;
  return u[1];
}

int
main ()
{
  u128 x = foo (0, (v64u128){ });
  if (x != 1)
    __builtin_abort();
  return 0;
}

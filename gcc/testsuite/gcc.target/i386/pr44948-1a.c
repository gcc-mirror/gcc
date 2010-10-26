/* PR target/44948 */
/* { dg-do run } */
/* { dg-options "-O -Wno-psabi -mtune=generic" } */
/* { dg-require-effective-target avx_runtime } */
/* { dg-additional-sources pr44948-1b.c } */

#pragma GCC target ("avx")

struct A { long b[8] __attribute__((aligned (32))); };
void foo (long double, struct A);

int
main (void)
{
  struct A a = { { 0, 1, 2, 3, 4, 5, 6, 7 } };
  foo (8.0L, a);
  return 0;
}

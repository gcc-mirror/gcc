/* PR target/44948 */
/* { dg-do run } */
/* { dg-options "-O -Wno-psabi -mno-sse -mtune=generic" } */
/* { dg-require-effective-target ilp32 } */
/* { dg-require-effective-target sse2_runtime } */
/* { dg-additional-sources pr44948-2b.c } */

#pragma GCC target ("sse2")

struct A
{ 
  float V4SF __attribute__ ((vector_size (16)));
};

int
main (void)
{
  struct A a = { { 0, 1, 2, 3 } };
  foo (8.0L, a, 8.0L);
  return 0;
}

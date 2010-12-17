/* { dg-do compile } */
/* { dg-options "-O -mno-sse -Wno-psabi -mtune=generic" } */

struct A
{ 
  float V4SF __attribute__ ((vector_size (16)));
};

void
foo (long double x, struct A y, long double z)
{
  int i;
  struct A a = { { 0, 1, 2, 3 } };

  if (x != 8.0L || z != 8.0L)
    __builtin_abort ();
  if (__builtin_memcmp (&a, &y, sizeof (a)))
    __builtin_abort ();
}

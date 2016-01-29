/* PR target/69551 */
/* { dg-do run { target sse_runtime } } */
/* { dg-options "-O2 -mno-sse2 -msse" } */

typedef unsigned char v16qi __attribute__ ((vector_size (16)));
typedef unsigned int v4si __attribute__ ((vector_size (16)));

char __attribute__ ((noinline, noclone))
test (v4si vec)
{
  vec[1] = 0x5fb856;
  return ((v16qi) vec)[0];
}

int
main ()
{
  char z = test ((v4si) { -1, -1, -1, -1 });

  if (z != -1)
    __builtin_abort ();
  return 0;
}

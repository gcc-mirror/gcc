/* { dg-do compile } */

typedef int U __attribute__ ((vector_size (16)));

int
foo (int i)
{
#if __SSE2__
  register
#endif
    U u
#if __SSE2__
      asm ("xmm0")
#endif
      ;
  return u[i];
}

/* { dg-additional-options "-fno-strict-aliasing" } */


/* PR target/103170 */
/* AARCH64 used to ICE on this for a typo in the string template. */
#define vector __attribute__((vector_size(4*sizeof(float))))

typedef vector float v4sf;

v4sf f(int t)
{
  float tt = *(float*)&t;
  asm("":"+r"(tt));
  return (v4sf){tt,tt,tt,tt};
}

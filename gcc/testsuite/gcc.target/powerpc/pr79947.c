/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-options "-Ofast -mno-powerpc-gfxopt -mcmpb -mno-vsx" } */

/* PR 79949: Compiler segmentation fault due to not having conditional move
   support for the target if the -mno-powerpc-gfxopt option is used.  */

float a, b;
void
c ()
{
  a = __builtin_sqrtf (b);
}

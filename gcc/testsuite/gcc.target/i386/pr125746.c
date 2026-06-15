/* { dg-do compile } */
/* { dg-options "-msse -mno-sse2" } */

void
foo()
{
  register short b __asm("%xmm1") = 0; /* { dg-error "register specified for 'b' isn't suitable for data type" } */

  asm("" : "+v"(b));
}

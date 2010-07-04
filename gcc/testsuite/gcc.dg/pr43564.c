/* { dg-options "-O0" } */
/* { dg-do compile } */

static inline __attribute__ ((__always_inline__))
unsigned __clz (unsigned input)
{
  unsigned output;
  __asm__ __volatile__ ("clz %0, %1":"=r" (output):"r" (input));
}
__attribute__ ((optimize ("O2")))
void foo ()
{
  unsigned a;
  unsigned b;
  a = __clz (b);
}

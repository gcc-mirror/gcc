/* { dg-do compile } */
/* { dg-options "-O2 -mno-avx" } */
#include <string.h>
static inline
__attribute__ ((cold)) void
my_cold_memset (void *a, int b,int c)
{
  memset (a,b,c);
}
void
t(void *a,int b,int c)
{
  if (a)
    my_cold_memset (a,b,40);
}

/* The IF conditional should be predicted as cold and my_cold_memset inlined
   for size expanding memset as rep; stosb.  */
/* { dg-final { scan-assembler "stosb" } } */

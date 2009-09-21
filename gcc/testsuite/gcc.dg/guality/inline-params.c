/* { dg-do run { xfail *-*-* } } */
/* IPA-SRA removes the argumet as dead, so we don't see their values.  */
/* { dg-options "-g -fno-ipa-sra" } */
#define GUALITY_DONT_FORCE_LIVE_AFTER -1

#ifndef STATIC_INLINE
#define STATIC_INLINE /*static*/
#endif


#include "guality.h"

struct a{
  struct b {int a;} b;
  struct c{ int a;} c;
};

__attribute__ ((always_inline)) static inline void
t1 (struct b *ab, int b)
{
  GUALCHKXPRVAL ("b", 0xbbb, 0);
  GUALCHKVAL (ab);
}
__attribute__ ((always_inline)) static inline void
t2 (struct c *ac, char *msg)
{
  GUALCHKVAL (ac);
  GUALCHKVAL (msg);
}
__attribute__ ((always_inline)) static inline void
t3 (struct a *a)
{
  t1(&a->b, 0xbbb);
  t2(&a->c, "test");
}
struct a a={{0},{1}};
int
main (int argc, char *argv[])
{
  t3(&a);
}

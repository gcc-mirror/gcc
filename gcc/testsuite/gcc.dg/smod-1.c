/* PR middle-end/18045 */
/* Contributed by Eric Botcazou <ebotcazou@libertysurf.fr> */

/* { dg-do run } */
/* { dg-options "-std=c99" } */
/* { dg-options "-std=c99 -mtune=i486" { target { i?86-*-* && ilp32 } } } */

#include <limits.h>

extern void abort(void);

long long smod16(long long x)
{
  return x % 16;
}

int main(void)
{
#if LLONG_MAX > 2147483647L
  if (smod16 (0xFFFFFFFF) != 0xF)
    abort ();
#endif

  return 0;
}

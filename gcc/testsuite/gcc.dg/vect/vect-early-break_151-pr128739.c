/* PR tree-optimization/128739 */
/* { dg-do run } */
/* { dg-add-options vect_early_break } */
/* { dg-require-effective-target vect_early_break_hw } */
/* { dg-require-effective-target vect_int } */
/* { dg-additional-options "-O3" } */

#include "tree-vect.h"

static unsigned char buf[256];

/* Allow constant propagation of LIM to expose the counted loop exit.  */
__attribute__ ((noinline)) static unsigned
rd (const unsigned char **pp, const unsigned char *lim)
{
  const unsigned char *p = *pp;
  unsigned int s = *p++;
  unsigned int len = s;

  while (s == 255)
    {
      if (p >= lim)
	return 0;

      s = *p++;
      len += s;
    }

  *pp = p;
  return len;
}

int
main (void)
{
  check_vect ();

#pragma GCC novector
  for (int i = 0; i < 100; i++)
    buf[31 + i] = 255;

  buf[131] = 7;

  const unsigned char *p = buf + 31;
  if (rd (&p, buf + 132) != 255 * 100 + 7 || p != buf + 132)
    __builtin_abort ();

  return 0;
}

/* { dg-final { scan-tree-dump "LOOP VECTORIZED" "vect" { target aarch64*-*-* } } } */

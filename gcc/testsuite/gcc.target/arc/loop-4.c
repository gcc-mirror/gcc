/* { dg-do assemble } */
/* { dg-do compile } */
/* { dg-options "-Os -fbranch-count-reg" } */


int fn1(void *p1, int p2, int p3)
{
  char *d = p1;
  do
    *d++ = p2;
  while (--p3);
  return *d;
}

/* { dg-final { scan-assembler "lp_count" } } */

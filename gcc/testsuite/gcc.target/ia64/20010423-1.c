/* { dg-do compile } */
/* { dg-options "-O2" } */

int __sync_fetch_and_add_si (int *, int);

inline unsigned int
bar (volatile unsigned int *mem, unsigned int val)
{
  return __sync_fetch_and_add_si((int *)mem, (int)val);
}

volatile unsigned int x;

void foo (unsigned short *a)
{
  *a = bar (&x, 1) + 1;
}

/* { dg-do compile } */
/* { dg-options "-O2" } */

typedef __seg_gs struct foo_s {
  int a[20];
} foo_t;

int sum(void)
{
  const foo_t *p = (const foo_t *)0x1234;
  int i, total=0;
  for (i=0; i<20; i++)
    total += p->a[i];
  return total;
}

/* { dg-final { scan-assembler "add*.\[ \t\]%gs:" } } */

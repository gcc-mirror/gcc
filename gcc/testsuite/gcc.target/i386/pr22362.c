/* PR target/22362 */
/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-require-effective-target ilp32 } */

register unsigned int reg0 __asm__ ("esi");
register unsigned int reg1 __asm__ ("edi");
register unsigned int reg2 __asm__ ("ebx");

static unsigned int
__attribute__((noinline))
foo (unsigned long *x, void *y, void *z)
{
  int i;

  for (i = 5; i > 0; i--)
    x[i] = (unsigned long) foo ((unsigned long *) x[i], y, z);
  return 0;
}

unsigned int
bar (void)
{
  return foo (0, 0, 0);
}

/* PR target/100936 */
/* { dg-do assemble } */
/* { dg-options "-O2" } */
/* { dg-require-effective-target nonpic } */

__seg_gs int var;

static int
*foo (void)
{
  int *addr;

  asm ("lea %p1, %0" : "=r"(addr) : "m"(var));

  return addr;
}

static int
bar (int *addr)
{
  int val;

  asm ("mov %%gs:%1, %0" : "=r"(val) : "m"(*addr));

  return val;
}

int
baz (void)
{
  int *addr = foo();
  int val = bar (addr);
  
  return val;
}

/* { dg-do run } */
/* { dg-options "-std=gnu99 -Os" } */

/* This is a massively distilled test case based upon
   mm/memory.c:unmap_vmas() in the Linux kernel when compiled
   on sparc64 for SMP which uses a global register as the
   base of the per-cpu variable area.

   Because of a bug in global register handling in the dataflow
   code, the loop-invariant pass would move 'expression(regval)'
   outside of the loop.  */

extern void exit(int);
extern void abort(void);

register unsigned long regval __asm__("g6");

extern void cond_resched(void);

unsigned int var;

static unsigned long expression(unsigned long v)
{
  unsigned long ret;

  __asm__("" : "=r" (ret) : "0" (0));
  return ret + v;
}

void func(unsigned long *pp)
{
  int i;

  for (i = 0; i < 56; i++) {
    cond_resched();
    *pp = expression(regval);
  }
}

void __attribute__((noinline)) cond_resched(void)
{
	regval++;
}

int main(void)
{
  unsigned long val;

  regval = 100;
  func(&val);
  if (val != 156)
    abort();
  exit(0);
}

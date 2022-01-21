/* { dg-do compile } */
/* { dg-require-effective-target arm_thumb1_ok } */
/* { dg-additional-options "-mcpu=cortex-m7 -mthumb -O2" }  */

typedef unsigned long long uint64_t;
struct timer {
 int active;
 uint64_t expire;
 void *arg;
};
int irq_disable();
void irq_restore(int);
static inline uint64_t h(const  uint64_t *p64)
{
 uint64_t tmp;
 asm(
  "ldrd %Q[r], %R[r], %[p]\n"
  : [r]"=lh"(tmp)
  : [p]"m"(*p64)
  : "memory"
 );
 return tmp;
}
uint64_t monotonic;
void timer_callout(timer *tmr, uint64_t nsec, void *arg)
{
 const int s = irq_disable();
 if (tmr->active)
   tmr->arg = arg;
 tmr->expire = h(&monotonic) + 100000 + (nsec == 1 ? 0 : nsec);
 irq_restore(s);
}

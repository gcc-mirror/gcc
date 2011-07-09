/* { dg-do assemble } */
/* { dg-options "-O2" } */

void 
bar1 () 
{
  char foo;
  asm volatile ("mov%z0 %1, %0": "=m" (foo): "iq" (-23));
  asm volatile ("add%z0 %1, %0": "+m" (foo): "iq" (23));
  asm volatile ("mov%z0 %1, %0": "=q" (foo): "iq" (-23));
  asm volatile ("add%z0 %1, %0": "+q" (foo): "iq" (23));
}

void
bar2 () 
{
  short foo;
  asm volatile ("mov%z0 %1, %0": "=m" (foo): "ir" (-23));
  asm volatile ("add%z0 %1, %0": "+m" (foo): "ir" (23));
  asm volatile ("mov%z0 %1, %0": "=r" (foo): "ir" (-23));
  asm volatile ("add%z0 %1, %0": "+r" (foo): "ir" (23));

  asm volatile ("pop%z0 %0": "=m" (foo));
  asm volatile ("pop%z0 %0": "=r" (foo));
}

void
bar3 () 
{
  int foo;
  asm volatile ("mov%z0 %1, %0": "=m" (foo): "ir" (-23));
  asm volatile ("add%z0 %1, %0": "+m" (foo): "ir" (23));
  asm volatile ("mov%z0 %1, %0": "=r" (foo): "ir" (-23));
  asm volatile ("add%z0 %1, %0": "+r" (foo): "ir" (23));

#ifndef __x86_64__
  if (sizeof (void *) == sizeof (int))
    {
      asm volatile ("pop%z0 %0": "=m" (foo));
      asm volatile ("pop%z0 %0": "=r" (foo));
    }
#endif
}

void
bar4 () 
{
  if (sizeof (void *) == sizeof (long long))
    {
      long long foo;
      asm volatile ("mov%z0 %1, %0": "=m" (foo): "er" (-23));
      asm volatile ("add%z0 %1, %0": "+m" (foo): "er" (23));
      asm volatile ("mov%z0 %1, %0": "=r" (foo): "er" (-23));
      asm volatile ("add%z0 %1, %0": "+r" (foo): "er" (23));

      asm volatile ("pop%z0 %0": "=m" (foo));
      asm volatile ("pop%z0 %0": "=r" (foo));
    }
}

/* This used to ICE due to a reload bug on s390*.  */

/* { dg-do compile { target s390*-*-* } } */
/* { dg-options "-O2" } */

int f (unsigned int);
void g (void *);

void test (void *p, void *dummy)
{
  unsigned int flags = 0;

  if (dummy)
    g (dummy);

  if (p)
    flags |= 0x80000000;

  asm volatile ("" : : : "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12");

  if (dummy)
    g (dummy);

  if (p) 
    {
      flags |= 0x20000000|0x80000000;

      if (!f (0))
        flags &= ~0x80000000;
    }

  f (flags);

  if (dummy)
    g (dummy);
}


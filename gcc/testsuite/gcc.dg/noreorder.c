/* { dg-do compile } */
/* { dg-options "-O2" } */

/* The function order in this file is opposite from what the cgraph
   topological sort would output. So we can check the order is preserved. */

extern void f2(int);
static int func2(void);

#ifndef NOREORDER
#define NOREORDER __attribute__((no_reorder))
#endif

asm("firstasm");

NOREORDER __attribute__((noipa)) int bozo(void)
{
  f2(3);
  func2();
}

asm("jukjuk");

NOREORDER __attribute__((noipa)) static int func1(void)
{
  f2(1);
}

asm("barbar");

NOREORDER __attribute__((noipa)) static int func2(void)
{
  func1();
}

asm("lastasm");

/* { dg-final { scan-assembler "firstasm.*bozo.*jukjuk.*func1.*barbar.*func2.*lastasm" } } */

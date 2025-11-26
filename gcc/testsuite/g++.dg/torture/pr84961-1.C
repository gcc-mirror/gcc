// PR c++/84961
// { dg-do compile }
// { dg-options "-std=gnu++17" }

short a;
volatile int b;
int c, d;

void
foo ()
{
  asm volatile ("" : "=r" (b = a));
}

void
bar ()
{
  asm volatile ("" : "=r" (++c, ++d, b = a));
}

void
baz ()
{
  asm volatile ("" : "=r" (--b));
}

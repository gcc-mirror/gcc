// { dg-do assemble  }
// GROUPS passed niklas i386
unsigned long foo(unsigned long x)
{
  return x & ~0104000;
}

// PR c++/55619
// { dg-do compile }

typedef int V __attribute__ ((vector_size (4 * sizeof (int))));

static const V C = { 0x201, 0, 0, 0 };
static const int D = 0x201;

void
f ()
{
  __asm volatile ("" : : "m" (C));
  __asm volatile ("" : : "m" (D));
}

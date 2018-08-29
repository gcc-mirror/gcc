/* Exercise that -Warray-bounds is handled correctly for subobjects.
   Test case derived from the halt_fast_timekeeper function in Linux
   kernel/time/timekeeping.c.
   { dg-do compile }
   { dg-options "-O2 -Warray-bounds=2 -Wno-stringop-overflow -ftrack-macro-expansion=0" }  */

struct A
{
  int i;
  void *p;
  int j;
};

struct B
{
  struct A a;

  int i;
};

void sink (void*);

static void halt_fast_timekeeper (struct B *b)
{
  static struct A a;

  struct A *pa = &b->a;

  __builtin_memcpy (&a, pa, sizeof *pa);   /* { dg-bogus "\\\[-Warray-bounds" } */
  sink (&a);
}

struct C { int i; struct B b; } c;

void timekeeping_suspend (void)
{
  struct B *p = &c.b;

  halt_fast_timekeeper (p);
}

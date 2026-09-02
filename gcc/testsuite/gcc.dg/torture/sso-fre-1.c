/* { dg-do run } */
/* { dg-require-effective-target le } */

/* Value numbering used to translate a reference through an aggregate copy
   by folding the components of the reference into a constant offset into
   the right-hand side of the copy.  A reversed storage order is a property
   of the component and not of its position, so it was lost in the process
   and the load of F7 below was resolved to the least significant bits of
   the byte instead of to its most significant bits.  */

struct __attribute__((scalar_storage_order("big-endian"))) S0
{
  char f7 : 3;
  char f6 : 5;
};

struct S1 { int a; struct S0 f3; char pad; short s; };

struct S1 g;
struct S1 *escape;

__attribute__((noipa)) int
foo (void)
{
  struct S1 l;
  struct S0 *q;

  /* Reversed bit order, so this gives f7 == 1 and f6 == 4.  */
  *(char *) &g.f3 = 0x24;
  l = g;
  /* Keep L addressable so that it is not scalarized away.  */
  escape = &l;
  q = (struct S0 *) ((char *) &l + __builtin_offsetof (struct S1, f3));
  return q->f7;
}

int
main (void)
{
  if (foo () != 1)
    __builtin_abort ();
  return 0;
}

// PR c++/93529
// P1009: Array size deduction in new-expressions
// { dg-do run { target c++11 } }

// When the array bound is deduced to 0, malloc(0) returns
// a non-dereferenceable pointer.
int *p0 = new int[]{};
int *p1 = new int[]{ 1 };
int *p2 = new int[]{ 1, 2, 3 };
char *c1 = new char[]{"foo"};
#if __cpp_aggregate_paren_init
int *q0 = new int[]();
int *q1 = new int[](1);
int *q2 = new int[](1, 2, 3);
char *d1 = new char[]("foo");
char *d2 = new char[4]("foo");
char *d3 = new char[]((("foo")));
#endif

struct Aggr { int a; int b; int c; };
Aggr *a1 = new Aggr[]{};
Aggr *a2 = new Aggr[]{ 1, 2, 3 };
Aggr *a3 = new Aggr[]{ 1, 2, 3, 4 };
Aggr *a4 = new Aggr[]{ { 1, 2, 3 } };
Aggr *a5 = new Aggr[]{ { 1 }, { 6, 7 } };
#if __cpp_designated_initializers
Aggr *a9 = new Aggr[]{ { .a = 1, .b = 2, .c = 3 } };
#endif
#if __cpp_aggregate_paren_init
Aggr *a6 = new Aggr[]();
Aggr *a7 = new Aggr[]({ 1, 2, 3 });
Aggr *a8 = new Aggr[]({ 1 }, { 6, 7 });
#endif

int
main ()
{
  if (p1[0] != 1 || p2[0] != 1 || p2[1] != 2 || p2[2] != 3)
    __builtin_abort ();
  if (__builtin_strcmp (c1, "foo"))
    __builtin_abort ();
  if (a2->a != 1 || a2->b != 2 || a2->c != 3)
    __builtin_abort ();
  if (a3[0].a != 1 || a3[0].b != 2 || a3[0].c != 3
      || a3[1].a != 4 || a3[1].b != 0 || a3[1].c != 0)
    __builtin_abort ();
  if (a4->a != 1 || a4->b != 2 || a4->c != 3)
    __builtin_abort ();
  if (a5[0].a != 1 || a5[0].b != 0 || a5[0].c != 0
      || a5[1].a != 6 || a5[1].b != 7 || a5[1].c != 0)
    __builtin_abort ();
#if __cpp_designated_initializers
  if (a9->a != 1 || a9->b != 2 || a9->c != 3)
    __builtin_abort ();
#endif
#if __cpp_aggregate_paren_init
  if (q1[0] != 1)
    __builtin_abort ();
  if (q2[0] != 1 || q2[1] != 2 || q2[2] != 3)
    __builtin_abort ();
  if (__builtin_strcmp (d1, "foo") || __builtin_strcmp (d2, "foo")
      || __builtin_strcmp (d3, "foo"))
    __builtin_abort ();
  if (a7[0].a != 1 || a7[0].b != 2 || a7[0].c != 3)
    __builtin_abort ();
  if (a8[0].a != 1 || a8[0].b != 0 || a8[0].c != 0
      || a8[1].a != 6 || a8[1].b != 7 || a8[1].c != 0)
    __builtin_abort ();
#endif
}

// PR 16254
// { dg-do compile }
// We lost a CLEANUP_POINT_EXPR, leading to a crash destroying temp of A.

struct A
{
  ~A ();
  A (int);
};

int bar (const A &);

void
foo (int i)
{
  int format[1] = { bar (A (i)) };
}

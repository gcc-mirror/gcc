/* { dg-do compile } */
/* { dg-options "-std=gnu99 -pedantic-errors -Wno-c99-c11-compat" } */

struct S { int i; struct { int a; }; };
_Noreturn void foo (void);
typedef int A;
typedef int A;
_Thread_local int i;
_Static_assert (1, "foo");
_Atomic int a;
_Alignas (int) int aa;
enum e { E = _Alignof (double) };

void
fn (int n)
{
  _Generic (n, int: 0);
}

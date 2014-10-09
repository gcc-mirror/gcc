/* { dg-do compile } */
/* { dg-options "-std=gnu11 -pedantic-errors -Wc99-c11-compat" } */

struct S { int i; struct { int a; }; }; /* { dg-warning "ISO C99 doesn.t support unnamed structs/unions" } */
_Noreturn void foo (void); /* { dg-warning "ISO C99 does not support ._Noreturn." } */
typedef int A;
typedef int A; /* { dg-warning "redefinition of typedef .A." } */
_Thread_local int i; /* { dg-warning "ISO C99 does not support ._Thread_local." } */
_Static_assert (1, "foo"); /* { dg-warning "ISO C99 does not support ._Static_assert." } */
_Atomic int a; /* { dg-warning "ISO C99 does not support the ._Atomic. qualifier" } */
_Alignas (int) int aa; /* { dg-warning "ISO C99 does not support ._Alignas." } */
enum e { E = _Alignof (double) }; /* { dg-warning "ISO C99 does not support ._Alignof." } */

void
fn (int n)
{
  _Generic (n, int: 0); /* { dg-warning "ISO C99 does not support ._Generic." } */
}

/* { dg-do compile } */
/* { dg-options "-std=gnu90 -pedantic-errors -Wc99-c11-compat" } */

struct S { int i; struct { int a; }; }; /* { dg-error "ISO C90 doesn.t support unnamed structs/unions" } */
_Noreturn void foo (void); /* { dg-error "ISO C90 does not support ._Noreturn." } */
typedef int A;
typedef int A; /* { dg-error "redefinition of typedef .A." } */
_Thread_local int i; /* { dg-error "ISO C90 does not support ._Thread_local." } */
_Static_assert (1, "foo"); /* { dg-error "ISO C90 does not support ._Static_assert." } */
_Atomic int a; /* { dg-error "ISO C90 does not support the ._Atomic. qualifier" } */
_Alignas (int) int aa; /* { dg-error "ISO C90 does not support ._Alignas." } */
enum e { E = _Alignof (double) }; /* { dg-error "ISO C90 does not support ._Alignof." } */

void
fn (int n)
{
  _Generic (n, int: 0); /* { dg-error "ISO C90 does not support ._Generic." } */
}

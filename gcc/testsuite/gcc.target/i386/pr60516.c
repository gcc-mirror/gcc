/* PR target/60516 */
/* { dg-do compile { target { ! x32 } } } */
/* { dg-options "-O2" } */

struct S { char c[65536]; };

__attribute__((ms_abi, thiscall)) void
foo (void *x, struct S y)
{
}

__attribute__((ms_abi, fastcall)) void
bar (void *x, void *y, struct S z)
{
}

__attribute__((ms_abi, stdcall)) void
baz (struct S x)
{
}

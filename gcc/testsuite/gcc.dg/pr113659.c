/* { dg-do compile } */
/* { dg-options "-O3 -fno-tree-sra" } */
/* { dg-additional-options "-msse4.1" { target { x86_64-*-* i?86-*-* } } } */

struct Foo {
  int *ptr;
};
int Baz(struct Foo first)
{
  while (first.ptr)
    if (*first.ptr++)
      return 0;
  __builtin_unreachable ();
}

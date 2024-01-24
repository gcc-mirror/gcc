/* { dg-do compile { target { struct_tail_call && { c || c++11 } } } } */
/* { dg-additional-options "-fdelayed-branch" { target sparc*-*-* } } */

struct str
{
  int a, b;
};
struct str
cstruct (int x)
{
  if (x < 10)
    L:
    [[gnu::musttail]] return cstruct (x + 1);
  return ((struct str){ x, 0 });
}

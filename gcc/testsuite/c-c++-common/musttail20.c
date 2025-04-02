/* { dg-do compile { target struct_musttail } } */
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
    __attribute__((musttail)) return cstruct (x + 1);	/* { dg-warning "'musttail' attribute ignored" "" { target c } } */
  return ((struct str){ x, 0 });
}

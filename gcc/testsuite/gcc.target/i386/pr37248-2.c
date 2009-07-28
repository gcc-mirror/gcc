/* PR middle-end/37248 */
/* { dg-do compile { target { ! default_packed } } } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

struct S
{
  unsigned char a : 1;
  unsigned char b : 1;
  unsigned char c : 1;
  unsigned int d : 26;
  unsigned char e : 1;
  unsigned char f : 1;
  unsigned char g : 1;
} s;

int
foo (struct S x)
{
  return x.a && x.g && x.b && x.f && x.c && x.e;
}

/* { dg-final { scan-tree-dump "& (3758096391|0x0e0000007)\[^\n\t\]*== (3758096391|0x0e0000007)" "optimized" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */

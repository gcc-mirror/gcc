/* { dg-do compile } */
/* { dg-options "-mcpu=xiangshan-nanhu" } */

struct {
  int a;
  short s;
  unsigned x : 18;
  unsigned y : 14;
} bf;

void
foo()
{
  bf.s ^= bf.y / __builtin_stdc_rotate_left(bf.x, 5);
}

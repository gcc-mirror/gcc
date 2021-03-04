/* PR middle-end/93235 */
/* { dg-do compile } */
/* { dg-options "-O2 -fno-strict-aliasing" } */

struct sfp16 { __fp16 f; };
struct sfp16
foo (short x)
{
  struct sfp16 a;
  *(short*)&a.f = x;
  return a;
}

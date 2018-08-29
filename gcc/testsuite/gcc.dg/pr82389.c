/* PR tree-optimization/82389 */
/* { dg-do compile { target lp64 } } */
/* { dg-options "-w -O3" } */

struct S { char s[0x40000000]; } s;

void
foo (struct S *p)
{
  char b[0x0ffffffff0000000L];
  *(struct S *)&b[0x0fffffffef000000L] = s;
  *p = *(struct S *)&b[0x0fffffffefffffffL];
}

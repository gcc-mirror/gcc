/* { dg-do compile { target powerpc*-*-darwin* } } */
/* This test explicitly checks for output that expects common.  */
/* { dg-additional-options "-fcommon" { target powerpc*-*-darwin* } } */
/* { dg-final { scan-assembler ".comm\[\t \]_x,12,2" } } */
/* { dg-final { scan-assembler-not ".space 7" } } */
/* PR 23071 */

struct Test {
  double D __attribute__((packed,aligned(4)));
  short X;
} x;

struct {
  char x;
  struct Test t;
} b = { 1, { 2, 3 } };

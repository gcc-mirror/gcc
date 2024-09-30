/* The zero sized bitfield.

   In this testcase, two slices are expected.  */

/* { dg-do compile } */
/* { dg-options "-O0 -gctf -dA" } */

/* { dg-final { scan-assembler-times "\[\t \]0x2\[\t \]+\[^\n\]*cts_type" 2 } } */

/* { dg-final { scan-assembler-times "\[\t \]0x5\[\t \]+\[^\n\]*cts_bits" 1 } } */
/* { dg-final { scan-assembler-times "\[\t \]0xa\[\t \]+\[^\n\]*cts_bits" 1 } } */

/* { dg-final { scan-assembler-times "ctm_name" 2 } } */
struct foo
{
  int a:5;
  unsigned:0;
  int b:10;
} foome;

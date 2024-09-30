/* Test compilation of struct type.

   In this testcase, two CTF_K_STRUCT records are expected
   struct a : ctt_info = 0x1a000004 (4 field members)
   struct b : ctt_into = 0x1a000002 (2 field members)
*/

/* { dg-do compile } */
/* { dg-options "-O0 -gctf -dA" } */

/* { dg-final { scan-assembler-times "\[\t \]0x1a000004\[\t \]+\[^\n\]*ctt_info" 1 } } */
/* { dg-final { scan-assembler-times "\[\t \]0x1a000002\[\t \]+\[^\n\]*ctt_info" 1 } } */
/* { dg-final { scan-assembler-times "ctm_name" 6 } } */

struct a
{
  int d1;
  int d2;
  float c;
  struct b
    {
      int time;
      int wall;
    } b1;
} my_a;

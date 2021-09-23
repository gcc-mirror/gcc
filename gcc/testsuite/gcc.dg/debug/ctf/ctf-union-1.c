/* CTF generation for union type.  */

/* { dg-do compile )  */
/* { dg-options "-O0 -gctf -dA" } */
/* { dg-final { scan-assembler-times "\[\t \]0x1e000004\[\t \]+\[^\n\]*ctt_info" 1 } } */
/* { dg-final { scan-assembler-times "ctm_name" 4 } } */

union c
{
  int c1;
  int c2;
  int c3;
  int c4;
} my_u_c;

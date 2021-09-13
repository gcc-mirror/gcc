/* Test BTF generation for union type.  */

/* { dg-do compile )  */
/* { dg-options "-O0 -gbtf -dA" } */

/* One union type with 4 members */
/* { dg-final { scan-assembler-times "\[\t \]0x5000004\[\t \]+\[^\n\]*btt_info" 1 } } */
/* { dg-final { scan-assembler-times "btm_name" 4 } } */

union onion
{
  int redness;
  char *name;
  unsigned short freshness;
  unsigned short flavor;
} my_onion;

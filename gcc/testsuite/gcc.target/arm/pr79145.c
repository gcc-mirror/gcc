/* { dg-do compile } */
/* { dg-skip-if "Test is specific to the iWMMXt" { arm*-*-* } { "-mcpu=*" } { "-mcpu=iwmmxt" } } */
/* { dg-skip-if "Test is specific to the iWMMXt" { arm*-*-* } { "-mabi=*" } { "-mabi=iwmmxt" } } */
/* { dg-skip-if "Test is specific to the iWMMXt" { arm*-*-* } { "-march=*" } { "-march=iwmmxt" } } */
/* { dg-skip-if "Test is specific to ARM mode" { arm*-*-* } { "-mthumb" } { "" } } */
/* { dg-require-effective-target arm32 } */
/* { dg-require-effective-target arm_iwmmxt_ok } */
/* { dg-options "-mcpu=iwmmxt" } */

int
main (void)
{
  volatile long long t1;
  t1 ^= 0x55;
  return 0;
}

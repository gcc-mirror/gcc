/* { dg-do assemble } */
/* { dg-options "-mthumb -Os -fdump-rtl-ira " }  */
/* { dg-require-effective-target arm_thumb1_ok } */
/* { dg-skip-if "do not test on armv4t" { *-*-* } { "-march=armv4t" } } */
/* { dg-additional-options "-march=armv5t" {target arm_arch_v5t_ok} } */

int foo (char *, char *, int);
int test (int d, char * out, char *in, int len)
{
  if (out != in)
    foo (out, in, len);
  return 0;
}
/* { dg-final { object-size text <= 20 } } */
/* { dg-final { scan-rtl-dump-not "Split live-range of register" "ira" } } */

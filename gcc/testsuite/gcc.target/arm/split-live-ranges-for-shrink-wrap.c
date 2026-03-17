/* { dg-options "-Os -fdump-rtl-ira " } */
/* { dg-require-effective-target arm_arch_v5t_thumb_ok } */
/* { dg-add-options arm_arch_v5t_thumb } */

int foo (char *, char *, int);
int test (int d, char * out, char *in, int len)
{
  if (out != in)
    foo (out, in, len);
  return 0;
}
/* { dg-final { scan-rtl-dump-not "Split live-range of register" "ira" } } */

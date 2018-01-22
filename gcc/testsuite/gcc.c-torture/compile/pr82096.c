/* { dg-require-effective-target arm_arch_v5t_ok { target arm*-*-* } } */
/* { dg-skip-if "Do not combine float-abi values" { arm*-*-* } { "-mfloat-abi=*" } { "-mfloat-abi=soft" } } */
/* { dg-additional-options "-march=armv5t -mthumb -mfloat-abi=soft" { target arm*-*-* } } */

static long long AL[24];

int
check_ok (void)
{
  return (__sync_bool_compare_and_swap (AL+1, 0x200000003ll, 0x1234567890ll));
}

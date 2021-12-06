/* { dg-do run } */
/* { dg-require-effective-target arm_pacbti_hw } */
/* { dg-options "-march=armv8.1-m.main+pacbti+fp -mbranch-protection=pac-ret -mthumb -mfloat-abi=hard" } */

#if !defined (__ARM_FEATURE_PAC_DEFAULT)
#error "Feature test macro __ARM_FEATURE_BTI_DEFAULT should be defined."
#endif

#if defined (__ARM_FEATURE_BTI_DEFAULT)
#error "Feature test macro __ARM_FEATURE_BTI_DEFAULT should be undefined."
#endif

int
main()
{
  if (__ARM_FEATURE_PAC_DEFAULT != 1)
    __builtin_abort ();

  return 0;
}

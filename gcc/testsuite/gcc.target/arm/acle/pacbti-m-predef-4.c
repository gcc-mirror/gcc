/* { dg-do run } */
/* { dg-require-effective-target arm_arch_v8_1m_main_pacbti_link } */
/* { dg-require-effective-target arm_pacbti_hw } */
/* { dg-options "-mbranch-protection=pac-ret -mfloat-abi=hard" } */
/* { dg-add-options arm_arch_v8_1m_main_pacbti } */

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

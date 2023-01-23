/* { dg-do compile } */
/* { dg-require-effective-target mbranch_protection_ok } */
/* { dg-options "-march=armv8.1-m.main+fp -mbranch-protection=pac-ret+leaf -mfloat-abi=hard --save-temps" } */
/* { dg-final { check-function-bodies "**" "" } } */

#if defined (__ARM_FEATURE_BTI_DEFAULT)
#error "Feature test macro __ARM_FEATURE_BTI_DEFAULT should be undefined."
#endif

#if !defined (__ARM_FEATURE_PAC_DEFAULT)
#error "Feature test macro __ARM_FEATURE_PAC_DEFAULT should be defined."
#endif

/*
**foo:
**	bti
**	...
*/
__attribute__((target("branch-protection=pac-ret+bti"), noinline))
int foo ()
{
  return 3;
}

/*
**main:
**	pac	ip, lr, sp
**	...
**	aut	ip, lr, sp
**	bx	lr
*/
int
main()
{
  return 1 + foo ();
}

/* { dg-final { scan-assembler "\.eabi_attribute 50, 1" } } */
/* { dg-final { scan-assembler "\.eabi_attribute 52, 1" } } */
/* { dg-final { scan-assembler-not "\.eabi_attribute 74" } } */
/* { dg-final { scan-assembler "\.eabi_attribute 76, 1" } } */

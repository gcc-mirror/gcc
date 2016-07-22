/* { dg-require-effective-target arm_arch_v5_ok } */
/* { dg-add-options arm_arch_v5 } */

#if __ARM_ARCH_ISA_THUMB
#error "__ARM_ARCH_ISA_THUMB defined for ARMv5"
#endif

int foo;

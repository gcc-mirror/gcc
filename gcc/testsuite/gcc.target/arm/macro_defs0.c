/* { dg-do compile } */
/* { dg-require-effective-target arm_arch_v7m_ok } */
/* { dg-options "" } */
/* { dg-add-options arm_arch_v7m } */

#ifdef __ARM_FP
#error __ARM_FP should not be defined
#endif

#ifdef __ARM_NEON_FP
#error __ARM_NEON_FP should not be defined
#endif

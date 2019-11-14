/* Test for #prama target macros.  */
/* { dg-do compile } */
/* { dg-require-effective-target arm_thumb1_ok } */
/* { dg-require-effective-target arm_arm_ok } */

#pragma GCC target ("thumb")

#ifndef __thumb__
#error "__thumb__ is not defined"
#endif

#ifdef __thumb2__
#ifndef __ARM_32BIT_STATE
#error  "__ARM_32BIT_STATE is not defined"
#endif
#else /* thumb1 */
#ifdef __ARM_32BIT_STATE
#error  "__ARM_32BIT_STATE is defined"
#endif
#endif /* thumb1 */

#pragma GCC target ("arm")

#ifdef __thumb__
#error "__thumb__ is defined"
#endif

#if defined (__thumb2__) || defined (__thumb1__)
#error "thumb is defined"
#endif 

#ifndef __ARM_32BIT_STATE
#error  "__ARM_32BIT_STATE is not defined"
#endif

#pragma GCC reset_options

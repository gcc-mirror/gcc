/* { dg-do compile } */
/* { dg-options "-O2" } */

#pragma GCC push_options
#pragma GCC target ("arch=armv8-a")

#pragma GCC push_options
#pragma GCC target ("arch=armv8-a+tme")
#ifndef __ARM_FEATURE_TME
#error "__ARM_FEATURE_TME is not defined but should be!"
#endif

#pragma GCC pop_options

#ifdef __ARM_FEATURE_TME
#error "__ARM_FEATURE_TME is defined but should not be!"
#endif

/* Test Armv8.6-A features.  */

#ifdef __ARM_FEATURE_MATMUL_INT8
#error "__ARM_FEATURE_MATMUL_INT8 is defined but should not be!"
#endif

#pragma GCC push_options
#pragma GCC target ("arch=armv8.6-a")
#ifndef __ARM_FEATURE_MATMUL_INT8
#error "__ARM_FEATURE_MATMUL_INT8 is not defined but should be!"
#endif
#pragma GCC pop_options

#pragma GCC push_options
#pragma GCC target ("arch=armv8.2-a+i8mm")
#ifndef __ARM_FEATURE_MATMUL_INT8
#error "__ARM_FEATURE_MATMUL_INT8 is not defined but should be!"
#endif
#pragma GCC pop_options

#pragma GCC push_options
#pragma GCC target ("arch=armv8.6-a+nosimd")
#ifdef __ARM_FEATURE_MATMUL_INT8
#error "__ARM_FEATURE_MATMUL_INT8 is defined but should not be!"
#endif
#pragma GCC pop_options

#pragma GCC push_options
#pragma GCC target ("arch=armv8.6-a+nofp")
#ifdef __ARM_FEATURE_MATMUL_INT8
#error "__ARM_FEATURE_MATMUL_INT8 is defined but should not be!"
#endif
#pragma GCC pop_options

#ifdef __ARM_FEATURE_BF16_SCALAR_ARITHMETIC
#error "__ARM_FEATURE_BF16_SCALAR_ARITHMETIC is defined but should not be!"
#endif
#ifdef __ARM_FEATURE_BF16_VECTOR_ARITHMETIC
#error "__ARM_FEATURE_BF16_VECTOR_ARITHMETIC is defined but should not be!"
#endif

#pragma GCC push_options
#pragma GCC target ("arch=armv8.6-a")
#ifndef __ARM_FEATURE_BF16_SCALAR_ARITHMETIC
#error "__ARM_FEATURE_BF16_SCALAR_ARITHMETIC is not defined but should be!"
#endif
#ifndef __ARM_FEATURE_BF16_VECTOR_ARITHMETIC
#error "__ARM_FEATURE_BF16_VECTOR_ARITHMETIC is not defined but should be!"
#endif
#pragma GCC pop_options

#pragma GCC push_options
#pragma GCC target ("arch=armv8.2-a+bf16")
#ifndef __ARM_FEATURE_BF16_SCALAR_ARITHMETIC
#error "__ARM_FEATURE_BF16_SCALAR_ARITHMETIC is not defined but should be!"
#endif
#ifndef __ARM_FEATURE_BF16_VECTOR_ARITHMETIC
#error "__ARM_FEATURE_BF16_VECTOR_ARITHMETIC is not defined but should be!"
#endif
#pragma GCC pop_options

#pragma GCC push_options
#pragma GCC target ("arch=armv8.2-a+bf16+nosimd")
#ifndef __ARM_FEATURE_BF16_SCALAR_ARITHMETIC
#error "__ARM_FEATURE_BF16_SCALAR_ARITHMETIC is not defined but should be!"
#endif
#ifdef __ARM_FEATURE_BF16_VECTOR_ARITHMETIC
#error "__ARM_FEATURE_BF16_VECTOR_ARITHMETIC is defined but should not be!"
#endif
#pragma GCC pop_options

#pragma GCC push_options
#pragma GCC target ("arch=armv8.6-a+nofp")
#ifdef __ARM_FEATURE_BF16_SCALAR_ARITHMETIC
#error "__ARM_FEATURE_BF16_SCALAR_ARITHMETIC is defined but should not be!"
#endif
#ifdef __ARM_FEATURE_BF16_VECTOR_ARITHMETIC
#error "__ARM_FEATURE_BF16_VECTOR_ARITHMETIC is defined but should not be!"
#endif
#pragma GCC pop_options

#pragma GCC pop_options

int
foo (int a)
{
  return a;
}

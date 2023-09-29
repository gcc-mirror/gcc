/* { dg-do run } */
/* { dg-require-effective-target dfp_hw } */
/* { dg-require-effective-target has_arch_pwr6 } */
/* { dg-options "-mhard-float -O2 -save-temps" } */

/* Test the decimal floating point quantize built-ins.  */

#define DEBUG 0

#ifdef DEBUG
#include <stdio.h>
#endif
#include <float.h>

void abort (void);

int main()
{
#define IMM2  2
#define IMM3  3
#define IMM4  4

  _Decimal64 srcA_dfp64, srcB_dfp64;
  _Decimal64 result_dfp64;
  _Decimal64 expected_result_dfp64;
  _Decimal128 srcA_dfp128, srcB_dfp128;
  _Decimal128 result_dfp128;
  _Decimal128 expected_result_dfp128;

  /* Third argument of quantize built-ins is the rounding mode value (RMC).
     
     RMC    Rounding Mode
     00     Round to nearest, ties to even
     01     Round toward 0
     10     Round to nearest, ties toward 0
     11     Round according to DRN      */


  /* Tests for quantize with 64-bit DFP variable.  */
  srcA_dfp64 = 100.0df;
  srcB_dfp64 = 300.456789df;
  expected_result_dfp64 = 300.5df;

  result_dfp64 = __builtin_dfp_quantize (srcA_dfp64, srcB_dfp64, 0x0);

  if (result_dfp64 != expected_result_dfp64)
#if DEBUG
    printf("DFP 64-bit quantize of variable, RMC = 0 result does not match expected result\n");
#else
    abort();
#endif

  srcA_dfp64 = 100.00df;
  srcB_dfp64 = 300.456789df;
  expected_result_dfp64 = 300.45df;

  result_dfp64 = __builtin_dfp_quantize (srcA_dfp64, srcB_dfp64, 0x1);

  if (result_dfp64 != expected_result_dfp64)
#if DEBUG
    printf("DFP 64-bit quantize of variable, RMC = 1 result does not match expected result\n");
#else
    abort();
#endif

  srcA_dfp64 = 100.001df;
  srcB_dfp64 = 3001.456789df;
  expected_result_dfp64 = 3001.457df;

  result_dfp64 = __builtin_dfp_quantize (srcA_dfp64, srcB_dfp64, 0x2);

  if (result_dfp64 != expected_result_dfp64)
#if DEBUG
    printf("DFP 64-bit quantize of variable, RMC = 2 result does not match expected result\n");
#else
    abort();
#endif

  /* Tests for 64-bit quantize with immediate value.  */

  srcB_dfp64 = 10.4567df;
  expected_result_dfp64 = 000.0df;

  result_dfp64 = __builtin_dfp_quantize (IMM2, srcB_dfp64, 0x0);

  if (result_dfp64 != expected_result_dfp64)
#if DEBUG
    printf("DFP 64-bit quantize immediate, RMC = 0 result does not match expected result\n");
#else
    abort();
#endif

  srcB_dfp64 = 104567.891df;
  expected_result_dfp64 = 100000.0df;

  result_dfp64 = __builtin_dfp_quantize (IMM4, srcB_dfp64, 0x1);

  if (result_dfp64 != expected_result_dfp64)
#if DEBUG
    printf("DFP 64-bit quantize immediate, RMC = 1 result does not match expected result\n");
#else
    abort();
#endif

  srcB_dfp64 = 109876.54321df;
  expected_result_dfp64 = 109900.0df;

  result_dfp64 = __builtin_dfp_quantize (IMM2, srcB_dfp64, 0x2);

  if (result_dfp64 != expected_result_dfp64)
#if DEBUG
    printf("DFP 64-bit quantize immediate, RMC = 2 result does not match expected result\n");
#else
    abort();
#endif

  /* Tests for quantize 128-bit DFP variable.  */
  srcA_dfp128 = 0.018df;
  srcB_dfp128 = 50000.18345df;
  expected_result_dfp128 = 50000.180df;

  result_dfp128 = __builtin_dfp_quantize (srcA_dfp128, srcB_dfp128, 0x0);
  
  if (result_dfp128 != expected_result_dfp128)
#if DEBUG
    printf("DFP 128-bit quantize variable, RMC = 0 result does not match expected result\n");
#else
    abort();
#endif

  srcA_dfp128 = 8.01df;
  srcB_dfp128 = 50000.18345df;
  expected_result_dfp128 = 50000.18df;

  result_dfp128 = __builtin_dfp_quantize (srcA_dfp128, srcB_dfp128, 0x1);
  
  if (result_dfp128 != expected_result_dfp128)
#if DEBUG
    printf("DFP 128-bit quantize variable, RMC = 1 result does not match expected result\n");
#else
    abort();
#endif

  srcA_dfp128 = 0.1234df;
  srcB_dfp128 = 50000.18346789df;
  expected_result_dfp128 = 50000.1800df;

  result_dfp128 = __builtin_dfp_quantize (srcA_dfp128, srcB_dfp128, 0x2);
  
  if (result_dfp128 != expected_result_dfp128)
#if DEBUG
    printf("DFP 128-bit quantize variable, RMC = 2 result does not match expected result\n");
#else
    abort();
#endif

  /* Tests for 128-bit quantize with immediate value.  */
  srcB_dfp128 = 1234.18345df;
  expected_result_dfp128 = 1200.0df;

  result_dfp128 = __builtin_dfp_quantize (IMM2, srcB_dfp128, 0x0);

  if (result_dfp128 != expected_result_dfp128)
#if DEBUG
    printf("DFP 128-bit quantize immediate, RMC = 0 result does not match expected result\n");
#else
    abort();
#endif

  srcB_dfp128 = 123456.18345df;
  expected_result_dfp128 = 120000.0df;

  result_dfp128 = __builtin_dfp_quantize (IMM4, srcB_dfp128, 0x1);

  if (result_dfp128 != expected_result_dfp128)
#if DEBUG
    printf("DFP 128-bit quantize immediate, RMC = 1 result does not match expected result\n");
#else
    abort();
#endif

  srcB_dfp128 = 12361834.5df;
  expected_result_dfp128 = 12362000.0df;

  result_dfp128 = __builtin_dfp_quantize (IMM3, srcB_dfp128, 0x2);

  if (result_dfp128 != expected_result_dfp128)
#if DEBUG
    printf("DFP 128-bit quantize immediate, RMC = 2 result does not match expected result\n");
#else
    abort();
#endif

    return 0;
}

/* { dg-final { scan-assembler-times {\mdqua\M}   3 } } */
/* { dg-final { scan-assembler-times {\mdquai\M}  3 } } */
/* { dg-final { scan-assembler-times {\mdquaq\M}  3 } } */
/* { dg-final { scan-assembler-times {\mdquaiq\M} 3 } } */

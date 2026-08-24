#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <math.h>

#ifndef CHECK_CPU_SUPPORTS
#define CHECK_CPU_SUPPORTS true
#endif

#ifndef ATTRIBUTE
#define ATTRIBUTE
#endif

ATTRIBUTE
__attribute__ ((noipa, noinline))
static __bf16
foo (float a)
{
  return a;
}

__attribute__ ((noipa, noinline))
static __bf16
CALC (float *a)
{
  uint32_t bits;
  memcpy (&bits, a, sizeof (bits));
  /* Flush input denormal to zero like vcvtneps2bf16.  */
  if ((bits & 0x7f800000) == 0)
    {
      bits >>= 16;
      bits &= 0x8000;
    }
  else
    {
      uint32_t rounding_bias = 0x7FFF + ((bits >> 16) & 1);
      bits += rounding_bias;
      bits >>= 16;
    }
  __bf16 bf16;
  uint16_t bfloat16_bits = (uint16_t) bits;
  memcpy (&bf16, &bfloat16_bits, sizeof (bf16));
  return bf16;
}

__attribute__ ((noipa, noinline))
void
do_test (void)
{
  float test_values[] =
    {
      0.0f, -0.0f, 1.0f, -1.0f, 0.5f, -0.5f, 1000.0f, -1000.0f,
      3.1415926f, -3.1415926f, 1e-8f, -1e-8f, 1.0e+38f, -1.0e+38f,
      1.0e-38f, -1.0e-38f, 1.170903e-38, -1.170903e-38, 1.175494e-38,
      -1.175494e-38, 1.170902e-38, 4.999295e-39, 1.175494e-38
    };
  size_t num_values = sizeof (test_values) / sizeof (test_values[0]);
  bool failed = false;

  for (size_t i = 0; i < num_values; ++i)
    {
      float original = test_values[i];
      __bf16 hw_bf16 = foo (original);
      __bf16 sw_bf16 = CALC (&original);

      /* Verify psrld $16, %0 == %0 >> 16 */
      if (memcmp (&hw_bf16, &sw_bf16, sizeof (__bf16)) != 0)
	{
	  fprintf (stderr, "float -> bf16 failed: %d, %e\n", i, original);
	  failed = true;
	}

      /* Reconstruct the float value from the __bf16 bits */
      uint16_t bf16_bits;
      memcpy (&bf16_bits, &hw_bf16, sizeof (bf16_bits));
      uint32_t reconstructed_bits = ((uint32_t) bf16_bits) << 16;
      float converted;
      memcpy (&converted, &reconstructed_bits, sizeof (converted));

      float diff = fabsf (original - converted);

      /* Expected Maximum Precision Loss */
      uint32_t orig_bits;
      memcpy (&orig_bits, &original, sizeof (orig_bits));
      int exponent = ((orig_bits >> 23) & 0xFF) - 127;
      float expected_loss = (exponent == -127)
                            ? ldexpf (1.0f, -126 - 7)
                            : ldexpf (1.0f, exponent - 7);
      if (diff > expected_loss)
	{
	  fprintf (stderr, "bf16 -> float failed: %d, %e\n", i, original);
	  failed = true;
	}
    }

  if (failed)
    abort ();
}

int
main (void)
{
  if (CHECK_CPU_SUPPORTS)
    do_test ();
  return 0;
}

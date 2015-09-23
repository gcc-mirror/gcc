/* { dg-skip-if "" { arm*-*-* } } */

#include "arm_neon.h"

void abort (void);

void
foo (void)
{
  /* Test vcvt_high_f32_f64.  */
  float32x2_t arg1;
  float64x2_t arg2;
  float32x4_t result;
  arg1 = vcreate_f32 (UINT64_C (0x3f0db5793f6e1892));
  arg2 = vcombine_f64 (vcreate_f64 (UINT64_C (0x3fe8e49d23fb575d)),
		       vcreate_f64 (UINT64_C (0x3fd921291b3df73e)));
  //  Expect: "result" = 3ec909483f4724e93f0db5793f6e1892
  result = vcvt_high_f32_f64 (arg1, arg2);
  float32_t got;
  float32_t exp;

  /* Lane 0.  */
  got = vgetq_lane_f32 (result, 0);
  exp = ((float32_t) 0.9300624132156372);
  if (((((exp / got) < ((float32_t) 0.999))
	 || ((exp / got) > ((float32_t) 1.001)))
     && (((exp - got) < ((float32_t) -1.0e-4))
	 || ((exp - got) > ((float32_t) 1.0e-4)))))
    abort ();

  /* Lane 1.  */
  got = vgetq_lane_f32 (result, 1);
  exp = ((float32_t) 0.5535503029823303);
  if (((((exp / got) < ((float32_t) 0.999))
	  || ((exp / got) > ((float32_t) 1.001)))
     && (((exp - got) < ((float32_t) -1.0e-4))
	   || ((exp - got) > ((float32_t) 1.0e-4)))))
    abort ();

  /* Lane 2.  */
  got = vgetq_lane_f32 (result, 2);
  exp = ((float32_t) 0.7779069617051665);
  if (((((exp / got) < ((float32_t) 0.999))
	  || ((exp / got) > ((float32_t) 1.001)))
      && (((exp - got) < ((float32_t) -1.0e-4))
	  || ((exp - got) > ((float32_t) 1.0e-4)))))
    abort ();

  /* Lane 3.  */
  got = vgetq_lane_f32 (result, 3);
  exp = ((float32_t) 0.3926489606891329);
  if (((((exp / got) < ((float32_t) 0.999))
	  || ((exp / got) > ((float32_t) 1.001)))
      && (((exp - got) < ((float32_t) -1.0e-4))
	  || ((exp - got) > ((float32_t) 1.0e-4)))))
    abort ();
}

void
bar (void)
{
  /* Test vcvt_high_f64_f32.  */
  float32x4_t arg1;
  float64x2_t result;
  arg1 = vcombine_f32 (vcreate_f32 (UINT64_C (0x3f7c5cf13f261f74)),
		       vcreate_f32 (UINT64_C (0x3e3a7bc03f6ccc1d)));
  //  Expect: "result" = 3fc74f78000000003fed9983a0000000
  result = vcvt_high_f64_f32 (arg1);

  float64_t got;
  float64_t exp;

  /* Lane 0.  */
  got = vgetq_lane_f64 (result, 0);
  exp = 0.9249895215034485;
  if (((((exp / got) < 0.999)
	 || ((exp / got) > 1.001))
     && (((exp - got) < -1.0e-4)
	 || ((exp - got) > 1.0e-4))))
    abort ();

  /* Lane 1.  */
  got = vgetq_lane_f64 (result, 1);
  exp = 0.1821126937866211;
  if (((((exp / got) < 0.999)
	  || ((exp / got) > 1.001))
      && (((exp - got) < -1.0e-4)
	  || ((exp - got) > 1.0e-4))))
    abort ();
}

int
main (int argc, char **argv)
{
  foo ();
  bar ();
  return 0;
}

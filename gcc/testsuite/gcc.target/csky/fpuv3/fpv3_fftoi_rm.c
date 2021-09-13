/* { dg-do compile } */
/* { dg-skip-if  "test is specific to ck860f"  { csky-*-* }  { "*" }  { "-mcpu=ck860*f* -mfloat-abi=hard" "-mcpu=ck860*f* -mhard-float"  }  }  */
/* { dg-options "-ffast-math" } */

int lfloor_float32 (float x)
{
  return __builtin_lfloorf (x);
}

int lfloor_float64 (double x)
{
  return __builtin_lfloor (x);
}


float floor_float32 (float x)
{
  return __builtin_floorf (x);
}

double floor_float64 (double x)
{
  return __builtin_floor (x);
}

/* { dg-final { scan-assembler "fftoi\.f32\.s32\.rni" } }*/
/* { dg-final { scan-assembler "fftoi\.f64\.s32\.rni" } }*/
/* { dg-final { scan-assembler "fftofi\.f32\.rni" } }*/
/* { dg-final { scan-assembler "fftofi\.f64\.rni" } }*/

int lceil_float32 (float x)
{
  return __builtin_lceilf (x);
}

int lceil_float64 (double x)
{
  return __builtin_lceil (x);
}


float ceil_float32 (float x)
{
  return __builtin_ceilf (x);
}

double ceil_float64 (double x)
{
  return __builtin_ceil (x);
}

/* { dg-final { scan-assembler "fftoi\.f32\.s32\.rpi" } }*/
/* { dg-final { scan-assembler "fftoi\.f64\.s32\.rpi" } }*/
/* { dg-final { scan-assembler "fftofi\.f32\.rpi" } }*/
/* { dg-final { scan-assembler "fftofi\.f64\.rpi" } }*/

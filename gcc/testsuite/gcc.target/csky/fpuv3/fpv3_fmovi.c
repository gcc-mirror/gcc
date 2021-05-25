/* { dg-do compile } */
/* { dg-skip-if  "test is specific to ck860f"  { csky-*-* }  { "*" }  { "-mcpu=ck860*f* -mfloat-abi=hard" "-mcpu=ck860*f* -mhard-float"  }  }  */
/* { dg-options "-O2 -mfpu=fpv3" } */

#define FUNC(i) func_ ## i

#define FTYPE __fp16
FTYPE FUNC(movi16) (void)
{
  return 1.0;
}

/* { dg-final { scan-assembler "fmovi\.16" } }*/

#undef FTYPE
#define FTYPE float
FTYPE FUNC(movi32) (void)
{
  return 1.0;
}

/* { dg-final { scan-assembler "fmovi\.32" } }*/

#undef FTYPE
#define FTYPE double
FTYPE FUNC(movi64) (void)
{
  return 1.0;
}

/* { dg-final { scan-assembler "fmovi\.64" } }*/

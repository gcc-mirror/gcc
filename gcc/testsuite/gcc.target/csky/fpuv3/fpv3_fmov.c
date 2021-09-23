/* { dg-do compile } */
/* { dg-skip-if  "test is specific to ck860f"  { csky-*-* }  { "*" }  { "-mcpu=ck860*f* -mfloat-abi=hard" "-mcpu=ck860*f* -mhard-float"  }  }  */
/* { dg-options "-O2 -mfpu=fpv3" } */

#define FUNC(i) func_ ## i

#define FTYPE __fp16
FTYPE FUNC(ld16) (FTYPE *a, FTYPE *b)
{
  return *a = *b;
}

/* { dg-final { scan-assembler "fld\.16" } }*/
/* { dg-final { scan-assembler "fst\.16" } }*/

#undef FTYPE
#define FTYPE float
FTYPE FUNC(ld32) (FTYPE *a, FTYPE *b)
{
  return *a = *b;
}

/* { dg-final { scan-assembler "fld\.32" } }*/
/* { dg-final { scan-assembler "fld\.32" } }*/

#undef FTYPE
#define FTYPE double
FTYPE FUNC(ld64) (FTYPE *a, FTYPE *b)
{
  return *a = *b;
}

/* { dg-final { scan-assembler "fld\.64" } }*/
/* { dg-final { scan-assembler "fst\.64" } }*/


#undef FTYPE
#define FTYPE __fp16
FTYPE FUNC(ldr16) (FTYPE *a, int i, FTYPE *b)
{
  return a[i] = b[i];
}

/* { dg-final { scan-assembler "fldr\.16" } }*/
/* { dg-final { scan-assembler "fstr\.16" } }*/

#undef FTYPE
#define FTYPE float
FTYPE FUNC(ldr32) (FTYPE *a, int i, FTYPE *b)
{
  return a[i] = b[i];
}

/* { dg-final { scan-assembler "fldr\.32" } }*/
/* { dg-final { scan-assembler "fstr\.32" } }*/

#undef FTYPE
#define FTYPE double
FTYPE FUNC(ldr64) (FTYPE *a, int i, FTYPE *b)
{
  return a[i] = b[i];
}

/* { dg-final { scan-assembler "fldr\.64" } }*/
/* { dg-final { scan-assembler "fstr\.64" } }*/


#undef FTYPE
#define FTYPE __fp16
FTYPE FUNC(mov16) (FTYPE a, FTYPE b)
{
  a = b;
  return a;
}

/* { dg-final { scan-assembler "fmov\.16" } }*/

#undef FTYPE
#define FTYPE float
FTYPE FUNC(mov32) (FTYPE a, FTYPE b)
{
  a = b;
  return a;
}

/* { dg-final { scan-assembler "fmov\.32" } }*/

#undef FTYPE
#define FTYPE double
FTYPE FUNC(mov64) (FTYPE a, FTYPE b)
{
  a = b;
  return a;
}

/* { dg-final { scan-assembler "fmov\.64" } }*/

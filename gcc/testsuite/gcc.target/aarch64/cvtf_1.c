/* { dg-do run } */
/* { dg-options "-save-temps -fno-inline -O1" } */

#define FCVTDEF(ftype,itype) \
void \
cvt_##itype##_to_##ftype (itype a, ftype b)\
{\
  ftype c;\
  c = (ftype) a;\
  if ( (c - b) > 0.00001) abort();\
}

#define force_simd_for_float(v) asm volatile ("mov %s0, %1.s[0]" :"=w" (v) :"w" (v) :)
#define force_simd_for_double(v) asm volatile ("mov %d0, %1.d[0]" :"=w" (v) :"w" (v) :)

#define FCVTDEF_SISD(ftype,itype) \
void \
cvt_##itype##_to_##ftype##_sisd (itype a, ftype b)\
{\
  ftype c;\
  force_simd_for_##ftype(a);\
  c = (ftype) a;\
  if ( (c - b) > 0.00001) abort();\
}

#define FCVT(ftype,itype,ival,fval) cvt_##itype##_to_##ftype (ival, fval);
#define FCVT_SISD(ftype,itype,ival,fval) cvt_##itype##_to_##ftype##_sisd (ival, fval);

typedef int int32_t;
typedef unsigned int uint32_t;
typedef long long int int64_t;
typedef unsigned long long int uint64_t;

extern void abort();

FCVTDEF (float, int32_t)
/* { dg-final { scan-assembler "scvtf\ts\[0-9\]+,\ w\[0-9\]+" } } */
FCVTDEF (float, uint32_t)
/* { dg-final { scan-assembler "ucvtf\ts\[0-9\]+,\ w\[0-9\]+" } } */
FCVTDEF (double, int32_t)
/* "scvtf\td\[0-9\]+,\ w\[0-9\]+" */
FCVTDEF (double, uint32_t)
/* "ucvtf\td\[0-9\]+,\ w\[0-9\]+" */
FCVTDEF (float, int64_t)
/* "scvtf\ts\[0-9\]+,\ x\[0-9\]+" */
FCVTDEF (float, uint64_t)
/* "ucvtf\ts\[0-9\]+,\ x\[0-9\]+" */
FCVTDEF (double, int64_t)
/* { dg-final { scan-assembler "scvtf\td\[0-9\]+,\ x\[0-9\]+" } } */
FCVTDEF (double, uint64_t)
/* { dg-final { scan-assembler "ucvtf\td\[0-9\]+,\ x\[0-9\]+" } } */
FCVTDEF_SISD (float, int32_t)
/* { dg-final { scan-assembler "scvtf\ts\[0-9\]+,\ s\[0-9\]+" } } */
FCVTDEF_SISD (double, int64_t)
/* { dg-final { scan-assembler "scvtf\td\[0-9\]+,\ d\[0-9\]+" } } */
FCVTDEF_SISD (float, uint32_t)
/* { dg-final { scan-assembler "ucvtf\ts\[0-9\]+,\ s\[0-9\]+" } } */
FCVTDEF_SISD (double, uint64_t)
/* { dg-final { scan-assembler "ucvtf\td\[0-9\]+,\ d\[0-9\]+" } } */
FCVTDEF_SISD (float, int64_t)
/* { dg-final { scan-assembler-times "scvtf\ts\[0-9\]+,\ x\[0-9\]+" 2 } } */
FCVTDEF_SISD (float, uint64_t)
/* { dg-final { scan-assembler-times "ucvtf\ts\[0-9\]+,\ x\[0-9\]+" 2 } } */
FCVTDEF_SISD (double, int32_t)
/* { dg-final { scan-assembler-times "scvtf\td\[0-9\]+,\ w\[0-9\]+" 2 } } */
FCVTDEF_SISD (double, uint32_t)
/* { dg-final { scan-assembler-times "ucvtf\td\[0-9\]+,\ w\[0-9\]+" 2 } } */

int32_t ival = -1234;
int64_t llival = -13031303L;
uint32_t uival = 1234;
uint64_t ullival = 13031303L;

int main ()
{
  float x;
  double y;

  FCVT (float, int32_t, ival, -1234.0);
  FCVT (float, uint32_t, uival, 1234.0);
  FCVT (float, int64_t, llival, -13031303.0);
  FCVT (float, uint64_t, ullival, 13031303.0);
  FCVT (double, int32_t, ival, -1234.0);
  FCVT (double, uint32_t, uival, 1234.0);
  FCVT (double, int64_t, llival, -13031303.0);
  FCVT (double, uint64_t, ullival, 13031303.0);
  FCVT_SISD (float, int32_t, ival, -1234.0);
  FCVT_SISD (double, int64_t, llival, -13031303.0);
  FCVT_SISD (float, uint32_t, uival, 1234.0);
  FCVT_SISD (double, uint64_t, ullival, 13031303.0);

  return 0;
}

/* { dg-final { cleanup-saved-temps } } */

/* { dg-do compile } */
/* { dg-options "-O2" } */

extern float fabsf (float);
extern float sqrtf (float);
extern double fabs (double);
extern double sqrt (double);

volatile float f1, f2, f3;
volatile int cond1, cond2;

void test_sf() {
  /* abssf2 */
  /* { dg-final { scan-assembler "fabs\ts\[0-9\]*" } } */
  f1 = fabsf (f1);
  /* negsf2 */
  /* { dg-final { scan-assembler "fneg\ts\[0-9\]*" } } */
  f1 = -f1;
  /* addsf3 */
  /* { dg-final { scan-assembler "fadd\ts\[0-9\]*" } } */
  f1 = f2 + f3;
  /* subsf3 */
  /* { dg-final { scan-assembler "fsub\ts\[0-9\]*" } } */
  f1 = f2 - f3;
  /* divsf3 */
  /* { dg-final { scan-assembler "fdiv\ts\[0-9\]*" } } */
  f1 = f2 / f3;
  /* mulsf3 */
  /* { dg-final { scan-assembler "fmul\ts\[0-9\]*" } } */
  f1 = f2 * f3;
  /* sqrtsf2 */
  /* { dg-final { scan-assembler "fsqrt\ts\[0-9\]*" } } */
  f1 = sqrtf (f1);
  /* cmpsf */
  /* { dg-final { scan-assembler "fcmpe\ts\[0-9\]*" } } */
  if (f1 < f2)
    cond1 = 1;
  else
    cond2 = 1;
}

volatile double d1, d2, d3;

void test_df() {
  /* absdf2 */
  /* { dg-final { scan-assembler "fabs\td\[0-9\]*" } } */
  d1 = fabs (d1);
  /* negdf2 */
  /* { dg-final { scan-assembler "fneg\td\[0-9\]*" } } */
  d1 = -d1;
  /* adddf3 */
  /* { dg-final { scan-assembler "fadd\td\[0-9\]*" } } */
  d1 = d2 + d3;
  /* subdf3 */
  /* { dg-final { scan-assembler "fsub\td\[0-9\]*" } } */
  d1 = d2 - d3;
  /* divdf3 */
  /* { dg-final { scan-assembler "fdiv\td\[0-9\]*" } } */
  d1 = d2 / d3;
  /* muldf3 */
  /* { dg-final { scan-assembler "fmul\td\[0-9\]*" } } */
  d1 = d2 * d3;
  /* sqrtdf2 */
  /* { dg-final { scan-assembler "fsqrt\td\[0-9\]*" } } */
  d1 = sqrt (d1);
  /* cmpdf */
  /* { dg-final { scan-assembler "fcmpe\td\[0-9\]*" } } */
  if (d1 < d2)
    cond1 = 1;
  else
    cond2 = 1;
}

volatile int i1;
volatile unsigned int u1;

void test_convert () {
  /* extendsfdf2 */
  /* { dg-final { scan-assembler "fcvt\td\[0-9\]*" } } */
  d1 = f1;
  /* truncdfsf2 */
  /* { dg-final { scan-assembler "fcvt\ts\[0-9\]*" } } */
  f1 = d1;
  /* fixsfsi2 */
  /* { dg-final { scan-assembler "fcvtzs\ts\[0-9\], s\[0-9\]*" } } */
  i1 = f1;
  /* fixdfsi2 */
  /* { dg-final { scan-assembler "fcvtzs\tw\[0-9\], d\[0-9\]*" } } */
  i1 = d1;
  /* fixunsfsi2 */
  /* { dg-final { scan-assembler "fcvtzu\ts\[0-9\], s\[0-9\]*" } } */
  u1 = f1;
  /* fixunsdfsi2 */
  /* { dg-final { scan-assembler "fcvtzu\tw\[0-9\], d\[0-9\]*" } } */
  u1 = d1;
  /* floatsisf2 */
  /* { dg-final { scan-assembler "scvtf\ts\[0-9\]*" } } */
  f1 = i1;
  /* floatsidf2 */
  /* { dg-final { scan-assembler "scvtf\td\[0-9\]*" } } */
  d1 = i1;
  /* floatunssisf2 */
  /* { dg-final { scan-assembler "ucvtf\ts\[0-9\]*" } } */
  f1 = u1;
  /* floatunssidf2 */
  /* { dg-final { scan-assembler "ucvtf\td\[0-9\]*" } } */
  d1 = u1;
}


/* { dg-do run } */
/* { dg-options "-O2 -ftree-vectorize -fno-inline -fno-vect-cost-model -save-temps" } */


extern void abort (void);
double fmax (double, double);
float fmaxf (float, float);
double fmin (double, double);
float fminf (float, float);

#define isnan __builtin_isnan
#define isinf __builtin_isinf

#define NAN __builtin_nan ("")
#define INFINITY __builtin_inf ()

#define NUM_ELEMS(TYPE) (16 / sizeof (TYPE))

#define DEF_MAXMIN(TYPE,FUN)\
void test_##FUN (TYPE *__restrict__ r, TYPE *__restrict__ a,\
		 TYPE *__restrict__ b)\
{\
  int i;\
  for (i = 0; i < NUM_ELEMS (TYPE); i++)\
    r[i] = FUN (a[i], b[i]);\
}\

DEF_MAXMIN (float, fmaxf)
DEF_MAXMIN (double, fmax)

DEF_MAXMIN (float, fminf)
DEF_MAXMIN (double, fmin)

int main ()
{
  float a_f[4] = { 4, NAN, -3, INFINITY };
  float b_f[4] = { 1,   7,NAN, 0 };
  float r_f[4];
  double a_d[4] = { 4, NAN,  -3,  INFINITY };
  double b_d[4] = { 1,   7, NAN,  0 };
  double r_d[4];

  test_fmaxf (r_f, a_f, b_f);
  if (r_f[0] != 4 || isnan (r_f[1]) || isnan (r_f[2]) || !isinf (r_f[3]))
    abort ();

  test_fminf (r_f, a_f, b_f);
  if (r_f[0] != 1 || isnan (r_f[1]) || isnan (r_f[2]) || isinf (r_f[3]))
    abort ();

  test_fmax (r_d, a_d, b_d);
  test_fmax (&r_d[2], &a_d[2], &b_d[2]);
  if (r_d[0] != 4 || isnan (r_d[1]) || isnan (r_d[2]) || !isinf (r_d[3]))
    abort ();

  test_fmin (r_d, a_d, b_d);
  test_fmin (&r_d[2], &a_d[2], &b_d[2]);
  if (r_d[0] != 1 || isnan (r_d[1]) || isnan (r_d[2]) || isinf (r_d[3]))
    abort ();

  return 0;
}

/* { dg-final { scan-assembler-times "fmaxnm\tv\[0-9\]+\.4s, v\[0-9\]+\.4s, v\[0-9\]+\.4s" 1 } } */
/* { dg-final { scan-assembler-times "fmaxnm\tv\[0-9\]+\.2d, v\[0-9\]+\.2d, v\[0-9\]+\.2d" 1 } } */

/* { dg-final { scan-assembler-times "fminnm\tv\[0-9\]+\.4s, v\[0-9\]+\.4s, v\[0-9\]+\.4s" 1 } } */
/* { dg-final { scan-assembler-times "fminnm\tv\[0-9\]+\.2d, v\[0-9\]+\.2d, v\[0-9\]+\.2d" 1 } } */


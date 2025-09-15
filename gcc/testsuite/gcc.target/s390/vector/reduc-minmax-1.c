/* { dg-do compile } */
/* { dg-options "-O3 -mzarch -march=z14 -ftree-vectorize -fdump-tree-optimized -fno-trapping-math" } */

#define MAX(a, b) ((a) > (b) ? (a) : (b))
#define MIN(a, b) ((a) > (b) ? (b) : (a))

/* unsigned integers */

unsigned char
reduce_umax_char (unsigned char *p)
{
  unsigned char res = p[0];
  for (int i = 0; i < 16; i++)
    res = MAX (res, p[i]);
  return res;
}

unsigned char
reduce_umin_char (unsigned char *p)
{
  unsigned char res = p[0];
  for (int i = 0; i < 16; i++)
    res = MIN (res, p[i]);
  return res;
}

unsigned short
reduce_umax_short (unsigned short *p)
{
  unsigned short res = p[0];
  for (int i = 0; i < 8; i++)
    res = MAX (res, p[i]);
  return res;
}

unsigned short
reduce_umin_short (unsigned short *p)
{
  unsigned short res = p[0];
  for (int i = 0; i < 8; i++)
    res = MIN (res, p[i]);
  return res;
}

unsigned int
reduce_umax_int (unsigned int* p)
{
  unsigned int res = p[0];
  for (int i = 0; i != 4; i++)
    res = MAX (res, p[i]);
  return res;
}

unsigned int
reduce_umin_int (unsigned int* p)
{
  unsigned int res = p[0];
  for (int i = 0; i != 4; i++)
    res = MIN(res, p[i]);
  return res;
}

unsigned long
reduce_umax_long (unsigned long* p)
{
  unsigned long res = p[0];
  for (int i = 0; i != 4; i++)
    res = MAX (res, p[i]);
  return res;
}

unsigned long
reduce_umin_long (unsigned long* p)
{
  unsigned long res = p[0];
  for (int i = 0; i != 4; i++)
    res = MIN(res, p[i]);
  return res;
}

/* signed integers */

signed char
reduce_smax_char (signed char *p)
{
  signed char res = p[0];
  for (int i = 0; i < 16; i++)
    res = MAX (res, p[i]);
  return res;
}

signed char
reduce_smin_char (signed char *p)
{
  signed char res = p[0];
  for (int i = 0; i < 16; i++)
    res = MIN (res, p[i]);
  return res;
}

signed short
reduce_smax_short (signed short *p)
{
  signed short res = p[0];
  for (int i = 0; i < 8; i++)
    res = MAX (res, p[i]);
  return res;
}

signed short
reduce_smin_short (signed short *p)
{
  signed short res = p[0];
  for (int i = 0; i < 8; i++)
    res = MIN (res, p[i]);
  return res;
}

signed int
reduce_smax_int (signed int* p)
{
  signed int res = p[0];
  for (int i = 0; i != 4; i++)
    res = MAX (res, p[i]);
  return res;
}

signed int
reduce_smin_int (signed int* p)
{
  signed int res = p[0];
  for (int i = 0; i != 4; i++)
    res = MIN(res, p[i]);
  return res;
}

signed long
reduce_smax_long (signed long* p)
{
  signed long res = p[0];
  for (int i = 0; i != 4; i++)
    res = MAX (res, p[i]);
  return res;
}

signed long
reduce_smin_long (signed long* p)
{
  signed long res = p[0];
  for (int i = 0; i != 4; i++)
    res = MIN(res, p[i]);
  return res;
}

float
__attribute__((optimize("Ofast")))
reduce_smax_float (float* p)
{
  float res = p[0];
  for (int i = 0; i != 4; i++)
    res = MAX (res, p[i]);
  return res;
}

float
__attribute__((optimize("Ofast")))
reduce_smin_float (float* p)
{
  float res = p[0];
  for (int i = 0; i != 4; i++)
    res = MIN (res, p[i]);
  return res;
}

double
__attribute__((optimize("Ofast")))
reduce_smax_double (double* p)
{
  double res = p[0];
  for (int i = 0; i != 4; i++)
    res = MAX (res, p[i]);
  return res;
}

double
__attribute__((optimize("Ofast")))
reduce_smin_double (double* p)
{
  double res = p[0];
  for (int i = 0; i != 4; i++)
    res = MIN (res, p[i]);
  return res;
}

float
reduce_fmax_float (float* p)
{
  float res = p[0];
  for (int i = 0; i != 4; i++)
    res = __builtin_fmaxf (res, p[i]);
  return res;
}

float
reduce_fmin_float (float* p)
{
  float res = p[0];
  for (int i = 0; i != 4; i++)
    res = __builtin_fminf (res, p[i]);
  return res;
}

double
reduce_fmax_double (double* p)
{
  double res = p[0];
  for (int i = 0; i != 4; i++)
    res = __builtin_fmax (res, p[i]);
  return res;
}

double
reduce_fmin_double (double* p)
{
  double res = p[0];
  for (int i = 0; i != 4; i++)
    res = __builtin_fmin (res, p[i]);
  return res;
}

/* { dg-final { scan-tree-dump-times "\.REDUC_MAX" 10 "optimized" } } */
/* { dg-final { scan-tree-dump-times "\.REDUC_MIN" 10 "optimized" } } */
/* { dg-final { scan-tree-dump-times "\.REDUC_FMAX" 2 "optimized" } } */
/* { dg-final { scan-tree-dump-times "\.REDUC_FMIN" 2 "optimized" } } */

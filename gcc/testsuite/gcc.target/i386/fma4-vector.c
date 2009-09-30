/* Test that the compiler properly optimizes floating point multiply and add
   instructions vector into vfmaddps on FMA4 systems.  */

/* { dg-do compile } */
/* { dg-require-effective-target lp64 } */
/* { dg-options "-O2 -mfma4 -ftree-vectorize" } */

extern void exit (int);

typedef float     __m128  __attribute__ ((__vector_size__ (16), __may_alias__));
typedef double    __m128d __attribute__ ((__vector_size__ (16), __may_alias__));

#define SIZE 10240

union {
  __m128 f_align;
  __m128d d_align;
  float f[SIZE];
  double d[SIZE];
} a, b, c, d;

void
flt_mul_add (void)
{
  int i;

  for (i = 0; i < SIZE; i++)
    a.f[i] = (b.f[i] * c.f[i]) + d.f[i];
}

void
dbl_mul_add (void)
{
  int i;

  for (i = 0; i < SIZE; i++)
    a.d[i] = (b.d[i] * c.d[i]) + d.d[i];
}

void
flt_mul_sub (void)
{
  int i;

  for (i = 0; i < SIZE; i++)
    a.f[i] = (b.f[i] * c.f[i]) - d.f[i];
}

void
dbl_mul_sub (void)
{
  int i;

  for (i = 0; i < SIZE; i++)
    a.d[i] = (b.d[i] * c.d[i]) - d.d[i];
}

void
flt_neg_mul_add (void)
{
  int i;

  for (i = 0; i < SIZE; i++)
    a.f[i] = (-(b.f[i] * c.f[i])) + d.f[i];
}

void
dbl_neg_mul_add (void)
{
  int i;

  for (i = 0; i < SIZE; i++)
    a.d[i] = (-(b.d[i] * c.d[i])) + d.d[i];
}

int main ()
{
  flt_mul_add ();
  flt_mul_sub ();
  flt_neg_mul_add ();

  dbl_mul_add ();
  dbl_mul_sub ();
  dbl_neg_mul_add ();
  exit (0);
}

/* { dg-final { scan-assembler "vfmaddps" } } */
/* { dg-final { scan-assembler "vfmaddpd" } } */
/* { dg-final { scan-assembler "vfmsubps" } } */
/* { dg-final { scan-assembler "vfmsubpd" } } */
/* { dg-final { scan-assembler "vfnmaddps" } } */
/* { dg-final { scan-assembler "vfnmaddpd" } } */

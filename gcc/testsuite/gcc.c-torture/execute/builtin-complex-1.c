/* Test for builtin conj, creal, cimag.  */
/* Origin: Joseph Myers <jsm28@cam.ac.uk> */

extern float _Complex conjf (float _Complex);
extern double _Complex conj (double _Complex);
extern long double _Complex conjl (long double _Complex);

extern float crealf (float _Complex);
extern double creal (double _Complex);
extern long double creall (long double _Complex);

extern float cimagf (float _Complex);
extern double cimag (double _Complex);
extern long double cimagl (long double _Complex);

extern void abort (void);
extern void exit (int);

extern void link_failure (void);

int
main (void)
{
  /* For each type, test both runtime and compile time (constant folding)
     optimization.  */
  volatile float _Complex fc = 1.0F + 2.0iF;
  volatile double _Complex dc = 1.0 + 2.0i;
  volatile long double _Complex ldc = 1.0L + 2.0iL;
  /* Test floats.  */
  if (conjf (fc) != 1.0F - 2.0iF)
    abort ();
  if (__builtin_conjf (fc) != 1.0F - 2.0iF)
    abort ();
  if (conjf (1.0F + 2.0iF) != 1.0F - 2.0iF)
    link_failure ();
  if (__builtin_conjf (1.0F + 2.0iF) != 1.0F - 2.0iF)
    link_failure ();
  if (crealf (fc) != 1.0F)
    abort ();
  if (__builtin_crealf (fc) != 1.0F)
    abort ();
  if (crealf (1.0F + 2.0iF) != 1.0F)
    link_failure ();
  if (__builtin_crealf (1.0F + 2.0iF) != 1.0F)
    link_failure ();
  if (cimagf (fc) != 2.0F)
    abort ();
  if (__builtin_cimagf (fc) != 2.0F)
    abort ();
  if (cimagf (1.0F + 2.0iF) != 2.0F)
    link_failure ();
  if (__builtin_cimagf (1.0F + 2.0iF) != 2.0F)
    link_failure ();
  /* Test doubles.  */
  if (conj (dc) != 1.0 - 2.0i)
    abort ();
  if (__builtin_conj (dc) != 1.0 - 2.0i)
    abort ();
  if (conj (1.0 + 2.0i) != 1.0 - 2.0i)
    link_failure ();
  if (__builtin_conj (1.0 + 2.0i) != 1.0 - 2.0i)
    link_failure ();
  if (creal (dc) != 1.0)
    abort ();
  if (__builtin_creal (dc) != 1.0)
    abort ();
  if (creal (1.0 + 2.0i) != 1.0)
    link_failure ();
  if (__builtin_creal (1.0 + 2.0i) != 1.0)
    link_failure ();
  if (cimag (dc) != 2.0)
    abort ();
  if (__builtin_cimag (dc) != 2.0)
    abort ();
  if (cimag (1.0 + 2.0i) != 2.0)
    link_failure ();
  if (__builtin_cimag (1.0 + 2.0i) != 2.0)
    link_failure ();
  /* Test long doubles.  */
  if (conjl (ldc) != 1.0L - 2.0iL)
    abort ();
  if (__builtin_conjl (ldc) != 1.0L - 2.0iL)
    abort ();
  if (conjl (1.0L + 2.0iL) != 1.0L - 2.0iL)
    link_failure ();
  if (__builtin_conjl (1.0L + 2.0iL) != 1.0L - 2.0iL)
    link_failure ();
  if (creall (ldc) != 1.0L)
    abort ();
  if (__builtin_creall (ldc) != 1.0L)
    abort ();
  if (creall (1.0L + 2.0iL) != 1.0L)
    link_failure ();
  if (__builtin_creall (1.0L + 2.0iL) != 1.0L)
    link_failure ();
  if (cimagl (ldc) != 2.0L)
    abort ();
  if (__builtin_cimagl (ldc) != 2.0L)
    abort ();
  if (cimagl (1.0L + 2.0iL) != 2.0L)
    link_failure ();
  if (__builtin_cimagl (1.0L + 2.0iL) != 2.0L)
    link_failure ();
  exit (0);
}

/* All the above cases should have been optimized to something else,
   even if not optimizing (unless -fno-builtin was specified).  So any
   remaining calls to the original functions should abort.  */

static float _Complex
conjf (float _Complex z)
{
  abort ();
}

static double _Complex
conj (double _Complex z)
{
  abort ();
}

static long double _Complex
conjl (long double _Complex z)
{
  abort ();
}

static float
crealf (float _Complex z)
{
  abort ();
}

static double
creal (double _Complex z)
{
  abort ();
}

static long double
creall (long double _Complex z)
{
  abort ();
}

static float
cimagf (float _Complex z)
{
  abort ();
}

static double
cimag (double _Complex z)
{
  abort ();
}

static long double
cimagl (long double _Complex z)
{
  abort ();
}

/* When optimizing, all the constant cases should have been
   constant folded, so no calls to link_failure should remain.  In any case,
   link_failure should not be called.  */

#ifndef __OPTIMIZE__
void
link_failure (void)
{
  abort ();
}
#endif

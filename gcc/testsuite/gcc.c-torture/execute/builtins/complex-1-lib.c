extern int inside_main;
extern void abort (void);
#ifdef __OPTIMIZE__
#define ABORT_INSIDE_MAIN do { if (inside_main) abort (); } while (0)
#else
#define ABORT_INSIDE_MAIN do { } while (0)
#endif

static float _Complex
conjf (float _Complex z)
{
  ABORT_INSIDE_MAIN;
  return ~z;
}

static double _Complex
conj (double _Complex z)
{
  ABORT_INSIDE_MAIN;
  return ~z;
}

static long double _Complex
conjl (long double _Complex z)
{
  ABORT_INSIDE_MAIN;
  return ~z;
}

static float
crealf (float _Complex z)
{
  ABORT_INSIDE_MAIN;
  return __real__ z;
}

static double
creal (double _Complex z)
{
  ABORT_INSIDE_MAIN;
  return __real__ z;
}

static long double
creall (long double _Complex z)
{
  ABORT_INSIDE_MAIN;
  return __real__ z;
}

static float
cimagf (float _Complex z)
{
  ABORT_INSIDE_MAIN;
  return __imag__ z;
}

static double
cimag (double _Complex z)
{
  ABORT_INSIDE_MAIN;
  return __imag__ z;
}

static long double
cimagl (long double _Complex z)
{
  ABORT_INSIDE_MAIN;
  return __imag__ z;
}

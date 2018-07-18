/* { dg-do run { target { powerpc*-*-* && lp64 } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-mvsx -O2" } */

/* This will run, and someday we should add the support to test whether we are
   running on VSX hardware.  */

#include <altivec.h>
#include <stdlib.h>

#ifdef DEBUG
#include <stdio.h>

static int errors = 0;
#endif

union args_d {
  double scalar[2];
  vector double vect;
};

union args_f {
  float scalar[4];
  vector float vect;
};

union largs {
  unsigned long scalar[2];
  vector bool long vect;
};

static void
do_test_d (union args_d *expected, union args_d *got, const char *name)
{
  if (expected->scalar[0] != got->scalar[0]
      || expected->scalar[1] != got->scalar[1])
    {
#ifdef DEBUG
      printf ("%s failed!\n", name);
      errors++;
#else
      abort ();
#endif
    }
}

static void
do_test_f (union args_f *expected, union args_f *got, const char *name)
{
  if (expected->scalar[0] != got->scalar[0]
      || expected->scalar[1] != got->scalar[1]
      || expected->scalar[2] != got->scalar[2]
      || expected->scalar[3] != got->scalar[3])
    {
#ifdef DEBUG
      printf ("%s failed!\n", name);
      errors++;
#else
      abort ();
#endif
    }
}

static void
do_ltest (union largs *expected, union largs *got, const char *name)
{
  if (expected->scalar[0] != got->scalar[0]
      || expected->scalar[1] != got->scalar[1])
    {
#ifdef DEBUG
      printf ("%s failed!\n", name);
      errors++;
#else
      abort ();
#endif
    }
}


/* Vec functions taking a single argument.  */
static vector double
vabs (vector double arg)
{
  return vec_abs (arg);
}

static vector double
vceil_d (vector double arg)
{
  return vec_ceil (arg);
}

static vector float
vceil_f (vector float arg)
{
  return vec_ceil (arg);
}

static vector double
vfloor_d (vector double arg)
{
  return vec_floor (arg);
}

static vector float
vfloor_f (vector float arg)
{
  return vec_floor (arg);
}

static vector double
vnearbyint_d (vector double arg)
{
  return vec_nearbyint (arg);
}

static vector float
vnearbyint_f (vector float arg)
{
  return vec_nearbyint (arg);
}

static vector float
vrint_f (vector float arg)
{
  return vec_rint (arg);
}

static vector double
vrint_d (vector double arg)
{
  return vec_rint (arg);
}

static vector float
vsqrt_f (vector float arg)
{
  return vec_sqrt (arg);
}

static vector double
vsqrt_d (vector double arg)
{
  return vec_sqrt (arg);
}

/* Single argument tests with double args  */
static struct
{
  union args_d result;
  union args_d input;
  vector double (*func) (vector double);
  const char *name;
} arg1_tests_d[] = {
  /* result		input			function	name */
  { {  1.0,  2.0 },	{ -1.0,  2.0 },		vabs,		"vabs" },
  { {  1.0,  2.0 },	{  1.0, -2.0 },		vabs,		"vabs" },
  { {  2.0,  2.0 },	{  1.1,  1.7 },		vceil_d,	"vceil_d" },
  { { -1.0, -1.0 },	{ -1.1, -1.7 },		vceil_d,	"vceil_d" },
  { { -1.0,  2.0 },	{ -1.5,  1.5 },		vceil_d,	"vceil_d" },
  { {  1.0,  1.0 },	{  1.1,  1.7 },		vfloor_d,	"vfloor_d" },
  { { -2.0, -2.0 },	{ -1.1, -1.7 },		vfloor_d,	"vfloor_d" },
  { { -2.0,  1.0 },	{ -1.5,  1.5 },		vfloor_d,	"vfloor_d" },
  { {  1.0,  2.0 },	{  1.1,  1.7 },		vnearbyint_d,	"vnearbyint_d" },
  { { -1.0, -2.0 },	{ -1.1, -1.7 },		vnearbyint_d,	"vnearbyint_d" },
  { { -2.0,  2.0 },	{ -1.5,  1.5 },		vnearbyint_d,	"vnearbyint_d" },
  { {  1.0,  2.0 },	{  1.1,  1.7 },		vrint_d,	"vrint_d" },
  { { -1.0, -2.0 },	{ -1.1, -1.7 },		vrint_d,	"vrint_d" },
  { { -2.0,  2.0 },	{ -1.5,  1.5 },		vrint_d,	"vrint_d" },

  { {  2.0,  4.0 },	{  4.0, 16.0 },		vsqrt_d,	"vsqrt_d" },
};

static void
test_arg1_d (void)
{
  unsigned i;

#ifdef DEBUG
  printf ("\nSingle argument tests with double args:\n");
#endif

  for (i = 0; i < sizeof (arg1_tests_d) / sizeof (arg1_tests_d[0]); i++)
    {
      union args_d u;
      u.vect = arg1_tests_d[i].func (arg1_tests_d[i].input.vect);

#ifdef DEBUG
      printf ("test %-16s: expected { %4g, %4g }, got { %4g, %4g }, input { %4g, %4g }\n",
	      arg1_tests_d[i].name,
	      arg1_tests_d[i].result.scalar[0],
	      arg1_tests_d[i].result.scalar[1],
	      u.scalar[0],
	      u.scalar[1],
	      arg1_tests_d[i].input.scalar[0],
	      arg1_tests_d[i].input.scalar[1]);
#endif

      do_test_d (&arg1_tests_d[i].result, &u, arg1_tests_d[i].name);
    }

  return;
}

/* Single argument tests with float args.  */
static struct
{
  union args_f result;
  union args_f input;
  vector float (*func) (vector float);
  const char *name;
} arg1_tests_f[] = {
  /* result			input				function	name */
  { { 2.0, 2.0, 3.0, 3.0 },     { 1.05, 1.1, 2.2, 2.3 },	vceil_f,	"vceil_f" },
  { { -1.0, -1.0, -2.0, -2.0 },	{ -1.1, -1.7, -2.1, -2.4 },	vceil_f,	"vceil_f" },
  { { 1.0, 1.0, 2.0, 2.0 },     { 1.05, 1.1, 2.2, 2.3 },	vfloor_f,	"vfloor_f" },
  { { -2.0, -2.0, -3.0, -3.0 },	{ -1.1, -1.7, -2.1, -2.4 },	vfloor_f,	"vfloor_f" },
  { {  1.0,  2.0, -3.0, 3.0 },	{  1.1,  1.7, -3.1, 3.1 },	vnearbyint_f,	"vnearbyint_f" },
  { { -1.0, -2.0, -3.0, 3.0 },	{ -1.1, -1.7, -2.9, 2.9 },	vnearbyint_f,	"vnearbyint_f" },
  { { -2.0,  2.0, -3.0, 3.0 },	{ -1.5,  1.5, -2.55, 3.49 },	vnearbyint_f,	"vnearbyint_f" },
  { {  10.0,  18.0, 30.0, 40.0 }, {  10.1,  17.7, 30.0, 40.01 }, vrint_f,	"vrint_f" },
  { { -11.0, -18.0, -30.0, -40.0 }, { -11.1, -17.7, -30.0, -40.01 }, vrint_f,	"vrint_f" },
  
  { {  2.0,  4.0 },	{  4.0, 16.0 },		vsqrt_f,	"vsqrt_f" },
};

static void
test_arg1_f (void)
{
  unsigned i;

#ifdef DEBUG
  printf ("\nSingle argument tests with float args:\n");
#endif

  for (i = 0; i < sizeof (arg1_tests_f) / sizeof (arg1_tests_f[0]); i++)
    {
      union args_f u;
      u.vect = arg1_tests_f[i].func (arg1_tests_f[i].input.vect);

#ifdef DEBUG
      printf ("test %-16s: expected { %4g, %4g, %4g, %4g }, got { %4g, %4g, %4g, %4g }, input { %4g, %4g, %4g, %4g }\n",
	      arg1_tests_f[i].name,
	      arg1_tests_f[i].result.scalar[0],
	      arg1_tests_f[i].result.scalar[1],
	      arg1_tests_f[i].result.scalar[2],
	      arg1_tests_f[i].result.scalar[3],
	      u.scalar[0],
	      u.scalar[1],
	      u.scalar[2],
	      u.scalar[3],
	      arg1_tests_f[i].input.scalar[0],
	      arg1_tests_f[i].input.scalar[1],
	      arg1_tests_f[i].input.scalar[2],
	      arg1_tests_f[i].input.scalar[3]);
#endif

      do_test_f (&arg1_tests_f[i].result, &u, arg1_tests_f[i].name);
    }

  return;
}


/* Vect functions taking 2 arguments.  */
static vector double
vadd (vector double arg1, vector double arg2)
{
  return vec_add (arg1, arg2);
}

static vector double
vadd2 (vector double arg1, vector double arg2)
{
  return arg1 + arg2;
}

static vector double
vsub (vector double arg1, vector double arg2)
{
  return vec_sub (arg1, arg2);
}

static vector double
vsub2 (vector double arg1, vector double arg2)
{
  return arg1 - arg2;
}

static vector double
vmul (vector double arg1, vector double arg2)
{
  return vec_mul (arg1, arg2);
}

static vector double
vmul2 (vector double arg1, vector double arg2)
{
  return arg1 * arg2;
}

static vector double
vdiv (vector double arg1, vector double arg2)
{
  return vec_div (arg1, arg2);
}

static vector double
vdiv2 (vector double arg1, vector double arg2)
{
  return arg1 / arg2;
}

static vector double
vmax (vector double arg1, vector double arg2)
{
  return vec_max (arg1, arg2);
}

static vector double
vmin (vector double arg1, vector double arg2)
{
  return vec_min (arg1, arg2);
}

/* 2 argument tests.  */
static struct
{
  union args_d result;
  union args_d input[2];
  vector double (*func) (vector double, vector double);
  const char *name;
} arg2_tests[] = {
  /* result */
  { {  4.0,  6.0 },	{ {  1.0,  2.0 }, {  3.0,  4.0 } },	vadd,	"vadd"  },
  { {  4.0, -6.0 },	{ {  1.0, -2.0 }, {  3.0, -4.0 } },	vadd,	"vadd"  },
  { {  4.0,  6.0 },	{ {  1.0,  2.0 }, {  3.0,  4.0 } },	vadd2,	"vadd2" },
  { {  4.0, -6.0 },	{ {  1.0, -2.0 }, {  3.0, -4.0 } },	vadd2,	"vadd2" },
  { { -2.0, -2.0 },	{ {  1.0,  2.0 }, {  3.0,  4.0 } },	vsub,	"vsub"  },
  { { -2.0,  2.0 },	{ {  1.0, -2.0 }, {  3.0, -4.0 } },	vsub,	"vsub"  },
  { { -2.0, -2.0 },	{ {  1.0,  2.0 }, {  3.0,  4.0 } },	vsub2,	"vsub2" },
  { { -2.0,  2.0 },	{ {  1.0, -2.0 }, {  3.0, -4.0 } },	vsub2,	"vsub2" },
  { {  6.0,  4.0 },	{ {  2.0,  8.0 }, {  3.0,  0.5 } },	vmul,	"vmul"  },
  { {  6.0,  4.0 },	{ {  2.0,  8.0 }, {  3.0,  0.5 } },	vmul2,	"vmul2" },
  { {  2.0,  0.5 },	{ {  6.0,  4.0 }, {  3.0,  8.0 } },	vdiv,	"vdiv"  },
  { {  2.0,  0.5 },	{ {  6.0,  4.0 }, {  3.0,  8.0 } },	vdiv2,	"vdiv2" },
  { {  3.0,  4.0 },	{ {  1.0,  2.0 }, {  3.0,  4.0 } },	vmax,	"vmax"  },
  { {  1.0,  4.0 },	{ {  1.0, -2.0 }, { -3.0,  4.0 } },	vmax,	"vmax"  },
  { {  1.0,  2.0 },	{ {  1.0,  2.0 }, {  3.0,  4.0 } },	vmin,	"vmin"  },
  { { -3.0, -2.0 },	{ {  1.0, -2.0 }, { -3.0,  4.0 } },	vmin,	"vmin"  },
};

static void
test_arg2 (void)
{
  unsigned i;

#ifdef DEBUG
  printf ("\nTwo argument tests:\n");
#endif

  for (i = 0; i < sizeof (arg2_tests) / sizeof (arg2_tests[0]); i++)
    {
      union args_d u;
      u.vect = arg2_tests[i].func (arg2_tests[i].input[0].vect,
				   arg2_tests[i].input[1].vect);

#ifdef DEBUG
      printf ("test %-16s: expected { %4g, %4g }, got { %4g, %4g }, input { %4g, %4g }, { %4g, %4g }\n",
	      arg2_tests[i].name,
	      arg2_tests[i].result.scalar[0],
	      arg2_tests[i].result.scalar[1],
	      u.scalar[0],
	      u.scalar[1],
	      arg2_tests[i].input[0].scalar[0],
	      arg2_tests[i].input[0].scalar[1],
	      arg2_tests[i].input[1].scalar[0],
	      arg2_tests[i].input[1].scalar[1]);
#endif

      do_test_d (&arg2_tests[i].result, &u, arg2_tests[i].name);
    }

  return;
}


/* Comparisons, returnning a boolean vector.  */
static vector bool long
vcmpeq (vector double arg1, vector double arg2)
{
  return vec_cmpeq (arg1, arg2);
}

static vector bool long
vcmplt (vector double arg1, vector double arg2)
{
  return vec_cmplt (arg1, arg2);
}

static vector bool long
vcmple (vector double arg1, vector double arg2)
{
  return vec_cmple (arg1, arg2);
}

static vector bool long
vcmpgt (vector double arg1, vector double arg2)
{
  return vec_cmpgt (arg1, arg2);
}

static vector bool long
vcmpge (vector double arg1, vector double arg2)
{
  return vec_cmpge (arg1, arg2);
}

#define ONE  0xffffffffffffffffUL
#define ZERO 0x0000000000000000UL

/* comparison tests.  */
static struct
{
  union largs result;
  union args_d input[2];
  vector bool long (*func) (vector double, vector double);
  const char *name;
} argcmp_tests[] = {
  { { ONE,  ZERO }, { {  1.0,  2.0 }, {  1.0, -2.0 } },	vcmpeq,	"vcmpeq" },
  { { ZERO, ONE  }, { { -1.0,  2.0 }, {  1.0,  2.0 } },	vcmpeq,	"vcmpeq" },

  { { ONE,  ONE  }, { {  1.0, -2.0 }, {  1.0, -2.0 } },	vcmple,	"vcmple" },
  { { ONE,  ONE  }, { {  1.0, -2.0 }, {  2.0, -1.0 } },	vcmple,	"vcmple" },
  { { ZERO, ZERO }, { {  2.0, -1.0 }, {  1.0, -2.0 } },	vcmple,	"vcmple" },

  { { ZERO, ZERO }, { {  1.0, -2.0 }, {  1.0, -2.0 } },	vcmplt,	"vcmplt" },
  { { ONE,  ONE  }, { {  1.0, -2.0 }, {  2.0, -1.0 } },	vcmplt,	"vcmplt" },
  { { ZERO, ZERO }, { {  2.0, -1.0 }, {  1.0, -2.0 } },	vcmplt,	"vcmplt" },

  { { ZERO, ZERO }, { {  1.0, -2.0 }, {  1.0, -2.0 } },	vcmpgt,	"vcmpgt" },
  { { ZERO, ZERO }, { {  1.0, -2.0 }, {  2.0, -1.0 } },	vcmpgt,	"vcmpgt" },
  { { ONE,  ONE  }, { {  2.0, -1.0 }, {  1.0, -2.0 } },	vcmpgt,	"vcmpgt" },

  { { ONE,  ONE  }, { {  1.0, -2.0 }, {  1.0, -2.0 } },	vcmpge,	"vcmpge" },
  { { ZERO, ZERO }, { {  1.0, -2.0 }, {  2.0, -1.0 } },	vcmpge,	"vcmpge" },
  { { ONE,  ONE  }, { {  2.0, -1.0 }, {  1.0, -2.0 } },	vcmpge,	"vcmpge" },
};

static void
test_argcmp (void)
{
  unsigned i;

#ifdef DEBUG
  printf ("\nComparison tests:\n");
#endif

  for (i = 0; i < sizeof (argcmp_tests) / sizeof (argcmp_tests[0]); i++)
    {
      union largs u;
      u.vect = argcmp_tests[i].func (argcmp_tests[i].input[0].vect,
				     argcmp_tests[i].input[1].vect);

#ifdef DEBUG
      printf ("test %-16s: expected { 0x%016lx, 0x%016lx }, got { 0x%016lx, 0x%016lx }, input { %4g, %4g }, { %4g, %4g }\n",
	      argcmp_tests[i].name,
	      argcmp_tests[i].result.scalar[0],
	      argcmp_tests[i].result.scalar[1],
	      u.scalar[0],
	      u.scalar[1],
	      argcmp_tests[i].input[0].scalar[0],
	      argcmp_tests[i].input[0].scalar[1],
	      argcmp_tests[i].input[1].scalar[0],
	      argcmp_tests[i].input[1].scalar[1]);
#endif

      do_ltest (&argcmp_tests[i].result, &u, argcmp_tests[i].name);
    }

  return;
}


int
main (int argc, char *argv[])
{
  test_arg1_f ();
  test_arg1_d ();
  test_arg2 ();
  test_argcmp ();

#ifdef DEBUG
  if (errors)
    {
      printf ("There were %d error(s)\n", errors);
      return errors;
    }
  else
    printf ("There were no errors\n");
#endif
  
  return 0;
}

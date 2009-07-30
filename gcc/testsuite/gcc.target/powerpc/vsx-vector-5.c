/* { dg-do compile { target { powerpc*-*-* && lp64 } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } { "*" } { "" } } */
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

union args {
  double scalar[2];
  vector double vect;
};

union largs {
  unsigned long scalar[2];
  vector bool long vect;
};

static void
do_test (union args *expected, union args *got, const char *name)
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
vceil (vector double arg)
{
  return vec_ceil (arg);
}

static vector double
vfloor (vector double arg)
{
  return vec_floor (arg);
}

static vector double
vnearbyint (vector double arg)
{
  return vec_nearbyint (arg);
}

static vector double
vrint (vector double arg)
{
  return vec_rint (arg);
}

static vector double
vsqrt (vector double arg)
{
  return vec_sqrt (arg);
}

/* Single argument tests.  */
static struct
{
  union args result;
  union args input;
  vector double (*func) (vector double);
  const char *name;
} arg1_tests[] = {
  /* result		input			function	name */
  { {  1.0,  2.0 },	{ -1.0,  2.0 },		vabs,		"vabs" },
  { {  1.0,  2.0 },	{  1.0, -2.0 },		vabs,		"vabs" },
  { {  2.0,  2.0 },	{  1.1,  1.7 },		vceil,		"vceil" },
  { { -1.0, -1.0 },	{ -1.1, -1.7 },		vceil,		"vceil" },
  { { -1.0,  2.0 },	{ -1.5,  1.5 },		vceil,		"vceil" },
  { {  1.0,  1.0 },	{  1.1,  1.7 },		vfloor,		"vfloor" },
  { { -2.0, -2.0 },	{ -1.1, -1.7 },		vfloor,		"vfloor" },
  { { -2.0,  1.0 },	{ -1.5,  1.5 },		vfloor,		"vfloor" },
  { {  1.0,  2.0 },	{  1.1,  1.7 },		vnearbyint,	"vnearbyint" },
  { { -1.0, -2.0 },	{ -1.1, -1.7 },		vnearbyint,	"vnearbyint" },
  { { -2.0,  2.0 },	{ -1.5,  1.5 },		vnearbyint,	"vnearbyint" },
  { {  1.0,  2.0 },	{  1.1,  1.7 },		vrint,		"vrint" },
  { { -1.0, -2.0 },	{ -1.1, -1.7 },		vrint,		"vrint" },
  { { -2.0,  2.0 },	{ -1.5,  1.5 },		vrint,		"vrint" },
  { {  2.0,  4.0 },	{  4.0, 16.0 },		vsqrt,		"vsqrt" },
};

static void
test_arg1 (void)
{
  unsigned i;

#ifdef DEBUG
  printf ("Single argument tests:\n");
#endif

  for (i = 0; i < sizeof (arg1_tests) / sizeof (arg1_tests[0]); i++)
    {
      union args u;
      u.vect = arg1_tests[i].func (arg1_tests[i].input.vect);

#ifdef DEBUG
      printf ("test %-16s: expected { %4g, %4g }, got { %4g, %4g }, input { %4g, %4g }\n",
	      arg1_tests[i].name,
	      arg1_tests[i].result.scalar[0],
	      arg1_tests[i].result.scalar[1],
	      u.scalar[0],
	      u.scalar[1],
	      arg1_tests[i].input.scalar[0],
	      arg1_tests[i].input.scalar[1]);
#endif

      do_test (&arg1_tests[i].result, &u, arg1_tests[i].name);
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
  union args result;
  union args input[2];
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
      union args u;
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

      do_test (&arg2_tests[i].result, &u, arg2_tests[i].name);
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
  union args input[2];
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
  test_arg1 ();
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

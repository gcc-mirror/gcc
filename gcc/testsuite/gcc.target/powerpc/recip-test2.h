/*
 * Included file to common source float/double checking
 * The following macros should be defined:
 *	TYPE	   -- floating point type
 *	NAME	   -- convert a name to include the type
 *	UNS_TYPE   -- type to hold TYPE as an unsigned number
 *	EXP_SIZE   -- size in bits of the exponent
 *	MAN_SIZE   -- size in bits of the mantissa
 *	UNS_ABS	   -- absolute value for UNS_TYPE
 *	FABS	   -- absolute value function for TYPE
 *	FMAX	   -- maximum function for TYPE
 *	FMIN	   -- minimum function for TYPE
 *	SQRT	   -- square root function for TYPE
 *	RMIN	   -- minimum random number to generate
 *	RMAX	   -- maximum random number to generate
 *	ASMDIV	   -- assembler instruction to do divide
 *	ASMSQRT	   -- assembler instruction to do square root
 *	BDIV	   -- # of bits of inaccuracy to allow for division
 *	BRSQRT	   -- # of bits of inaccuracy to allow for 1/sqrt
 *	INIT_DIV   -- Initial values to test 1/x against
 *	INIT_RSQRT -- Initial values to test 1/sqrt(x) against
 */

typedef union
{
  UNS_TYPE i;
  TYPE x;
} NAME (union);

/*
 * Input/output arrays.
 */

static NAME (union) NAME (div_input)  [] __attribute__((__aligned__(32))) = INIT_DIV;
static NAME (union) NAME (rsqrt_input)[] __attribute__((__aligned__(32))) = INIT_RSQRT;

#define DIV_SIZE   (sizeof (NAME (div_input))   / sizeof (TYPE))
#define RSQRT_SIZE (sizeof (NAME (rsqrt_input)) / sizeof (TYPE))

static TYPE NAME (div_expected)[DIV_SIZE] __attribute__((__aligned__(32)));
static TYPE NAME (div_output)  [DIV_SIZE] __attribute__((__aligned__(32)));

static TYPE NAME (rsqrt_expected)[RSQRT_SIZE] __attribute__((__aligned__(32)));
static TYPE NAME (rsqrt_output)  [RSQRT_SIZE] __attribute__((__aligned__(32)));


/*
 * Crack a floating point number into sign bit, exponent, and mantissa.
 */

static void
NAME (crack) (TYPE number, unsigned int *p_sign, unsigned *p_exponent, UNS_TYPE *p_mantissa)
{
  NAME (union) u;
  UNS_TYPE bits;

  u.x = number;
  bits = u.i;

  *p_sign = (unsigned int)((bits >> (EXP_SIZE + MAN_SIZE)) & 0x1);
  *p_exponent = (unsigned int)((bits >> MAN_SIZE) & ((((UNS_TYPE)1) << EXP_SIZE) - 1));
  *p_mantissa = bits & ((((UNS_TYPE)1) << MAN_SIZE) - 1);
  return;
}


/*
 * Prevent optimizer from eliminating + 0.0 to remove -0.0.
 */

volatile TYPE NAME (math_diff_0) = ((TYPE) 0.0);

/*
 * Return negative if two numbers are significanly different or return the
 * number of bits that are different in the mantissa.
 */

static int
NAME (math_diff) (TYPE a, TYPE b, int bits)
{
  TYPE zero = NAME (math_diff_0);
  unsigned int sign_a, sign_b;
  unsigned int exponent_a, exponent_b;
  UNS_TYPE mantissa_a, mantissa_b, diff;
  int i;

  /* eliminate signed zero.  */
  a += zero;
  b += zero;

  /* special case Nan.  */
  if (__builtin_isnan (a))
    return (__builtin_isnan (b) ? 0 : -1);

  if (a == b)
    return 0;

  /* special case infinity.  */
  if (__builtin_isinf (a))
    return (__builtin_isinf (b) ? 0 : -1);

  /* punt on denormal numbers.  */
  if (!__builtin_isnormal (a) || !__builtin_isnormal (b))
    return -1;

  NAME (crack) (a, &sign_a, &exponent_a, &mantissa_a);
  NAME (crack) (b, &sign_b, &exponent_b, &mantissa_b);

  /* If the sign is different, there is no hope.  */
  if (sign_a != sign_b)
    return -1;

  /* If the exponent is off by 1, see if the values straddle the power of two,
     and adjust things to do the mantassa check if we can.  */
  if ((exponent_a == (exponent_b+1)) || (exponent_a == (exponent_b-1)))
    {
      TYPE big = FMAX (a, b);
      TYPE small = FMIN (a, b);
      TYPE diff = FABS (a - b);
      unsigned int sign_big, sign_small, sign_test;
      unsigned int exponent_big, exponent_small, exponent_test;
      UNS_TYPE mantissa_big, mantissa_small, mantissa_test;

      NAME (crack) (big, &sign_big, &exponent_big, &mantissa_big);
      NAME (crack) (small, &sign_small, &exponent_small, &mantissa_small);

      NAME (crack) (small - diff, &sign_test, &exponent_test, &mantissa_test);
      if ((sign_test == sign_small) && (exponent_test == exponent_small))
	{
	  mantissa_a = mantissa_small;
	  mantissa_b = mantissa_test;
	}

      else
	{
	  NAME (crack) (big + diff, &sign_test, &exponent_test, &mantissa_test);
	  if ((sign_test == sign_big) && (exponent_test == exponent_big))
	    {
	      mantissa_a = mantissa_big;
	      mantissa_b = mantissa_test;
	    }

	  else
	    return -1;
	}
    }

  else if (exponent_a != exponent_b)
    return -1;

  diff = UNS_ABS (mantissa_a - mantissa_b);
  for (i = MAN_SIZE; i > 0; i--)
    {
      if ((diff & ((UNS_TYPE)1) << (i-1)) != 0)
	return i;
    }

  return -1;
}


/*
 * Turn off inlining to make code inspection easier.
 */

static void NAME (asm_div) (void) __attribute__((__noinline__));
static void NAME (vector_div) (void) __attribute__((__noinline__));
static void NAME (scalar_div) (void) __attribute__((__noinline__));
static void NAME (asm_rsqrt) (void) __attribute__((__noinline__));
static void NAME (vector_rsqrt) (void) __attribute__((__noinline__));
static void NAME (scalar_rsqrt) (void) __attribute__((__noinline__));
static void NAME (check_div) (const char *) __attribute__((__noinline__));
static void NAME (check_rsqrt) (const char *) __attribute__((__noinline__));
static void NAME (run) (void) __attribute__((__noinline__));


/*
 * Division function that might be vectorized.
 */

static void
NAME (vector_div) (void)
{
  size_t i;

  for (i = 0; i < DIV_SIZE; i++)
    NAME (div_output)[i] = ((TYPE) 1.0) / NAME (div_input)[i].x;
}

/*
 * Division function that is not vectorized.
 */

static void
NAME (scalar_div) (void)
{
  size_t i;

  for (i = 0; i < DIV_SIZE; i++)
    {
      TYPE x = ((TYPE) 1.0) / NAME (div_input)[i].x;
      TYPE y;
      __asm__ ("" : "=d" (y) : "0" (x));
      NAME (div_output)[i] = y;
    }
}

/*
 * Generate the division instruction via asm.
 */

static void
NAME (asm_div) (void)
{
  size_t i;

  for (i = 0; i < DIV_SIZE; i++)
    {
      TYPE x;
      __asm__ (ASMDIV " %0,%1,%2"
	       : "=d" (x)
	       : "d" ((TYPE) 1.0), "d" (NAME (div_input)[i].x));
      NAME (div_expected)[i] = x;
    }
}

/*
 * Reciprocal square root function that might be vectorized.
 */

static void
NAME (vector_rsqrt) (void)
{
  size_t i;

  for (i = 0; i < RSQRT_SIZE; i++)
    NAME (rsqrt_output)[i] = ((TYPE) 1.0) / SQRT (NAME (rsqrt_input)[i].x);
}

/*
 * Reciprocal square root function that is not vectorized.
 */

static void
NAME (scalar_rsqrt) (void)
{
  size_t i;

  for (i = 0; i < RSQRT_SIZE; i++)
    {
      TYPE x = ((TYPE) 1.0) / SQRT (NAME (rsqrt_input)[i].x);
      TYPE y;
      __asm__ ("" : "=d" (y) : "0" (x));
      NAME (rsqrt_output)[i] = y;
    }
}

/*
 * Generate the 1/sqrt instructions via asm.
 */

static void
NAME (asm_rsqrt) (void)
{
  size_t i;

  for (i = 0; i < RSQRT_SIZE; i++)
    {
      TYPE x;
      TYPE y;
      __asm__ (ASMSQRT " %0,%1" : "=d" (x) : "d" (NAME (rsqrt_input)[i].x));
      __asm__ (ASMDIV " %0,%1,%2" : "=d" (y) : "d" ((TYPE) 1.0), "d" (x));
      NAME (rsqrt_expected)[i] = y;
    }
}


/*
 * Functions to abort or report errors.
 */

static int NAME (error_count) = 0;

#ifdef VERBOSE
static int NAME (max_bits_div)   = 0;
static int NAME (max_bits_rsqrt) = 0;
#endif


/*
 * Compare the expected value with the value we got.
 */

static void
NAME (check_div) (const char *test)
{
  size_t i;
  int b;

  for (i = 0; i < DIV_SIZE; i++)
    {
      TYPE exp = NAME (div_expected)[i];
      TYPE out = NAME (div_output)[i];
      b = NAME (math_diff) (exp, out, BDIV);

#ifdef VERBOSE
      if (b != 0)
	{
	  NAME (union) u_in = NAME (div_input)[i];
	  NAME (union) u_exp;
	  NAME (union) u_out;
	  char explanation[64];
	  const char *p_exp;

	  if (b < 0)
	    p_exp = "failed";
	  else
	    {
	      p_exp = explanation;
	      sprintf (explanation, "%d bit error%s", b, (b > BDIV) ? ", failed" : "");
	    }

	  u_exp.x = exp;
	  u_out.x = out;
	  printf ("%s %s %s for 1.0 / %g [0x%llx], expected %g [0x%llx], got %g [0x%llx]\n",
		  TNAME (TYPE), test, p_exp,
		  (double) u_in.x, (unsigned long long) u_in.i,
		  (double) exp,    (unsigned long long) u_exp.i,
		  (double) out,    (unsigned long long) u_out.i);
	}
#endif

      if (b < 0 || b > BDIV)
	NAME (error_count)++;

#ifdef VERBOSE
      if (b > NAME (max_bits_div))
	NAME (max_bits_div) = b;
#endif
    }
}

static void
NAME (check_rsqrt) (const char *test)
{
  size_t i;
  int b;

  for (i = 0; i < RSQRT_SIZE; i++)
    {
      TYPE exp = NAME (rsqrt_expected)[i];
      TYPE out = NAME (rsqrt_output)[i];
      b = NAME (math_diff) (exp, out, BRSQRT);

#ifdef VERBOSE
      if (b != 0)
	{
	  NAME (union) u_in = NAME (rsqrt_input)[i];
	  NAME (union) u_exp;
	  NAME (union) u_out;
	  char explanation[64];
	  const char *p_exp;

	  if (b < 0)
	    p_exp = "failed";
	  else
	    {
	      p_exp = explanation;
	      sprintf (explanation, "%d bit error%s", b, (b > BDIV) ? ", failed" : "");
	    }

	  u_exp.x = exp;
	  u_out.x = out;
	  printf ("%s %s %s for 1 / sqrt (%g) [0x%llx], expected %g [0x%llx], got %g [0x%llx]\n",
		  TNAME (TYPE), test, p_exp,
		  (double) u_in.x, (unsigned long long) u_in.i,
		  (double) exp,    (unsigned long long) u_exp.i,
		  (double) out,    (unsigned long long) u_out.i);
	}
#endif

      if (b < 0 || b > BRSQRT)
	NAME (error_count)++;

#ifdef VERBOSE
      if (b > NAME (max_bits_rsqrt))
	NAME (max_bits_rsqrt) = b;
#endif
    }
}


/*
 * Now do everything.
 */

static void
NAME (run) (void)
{
#ifdef VERBOSE
  printf ("start run_%s, divide size = %ld, rsqrt size = %ld, %d bit%s for a/b, %d bit%s for 1/sqrt(a)\n",
	  TNAME (TYPE),
	  (long)DIV_SIZE,
	  (long)RSQRT_SIZE,
	  BDIV, (BDIV == 1) ? "" : "s",
	  BRSQRT, (BRSQRT == 1) ? "" : "s");
#endif

  NAME (asm_div) ();

  NAME (scalar_div) ();
  NAME (check_div) ("scalar");

  NAME (vector_div) ();
  NAME (check_div) ("vector");

  NAME (asm_rsqrt) ();

  NAME (scalar_rsqrt) ();
  NAME (check_rsqrt) ("scalar");

  NAME (vector_rsqrt) ();
  NAME (check_rsqrt) ("vector");

#ifdef VERBOSE
  printf ("end run_%s, errors = %d, max div bits = %d, max rsqrt bits = %d\n",
	  TNAME (TYPE),
	  NAME (error_count),
	  NAME (max_bits_div),
	  NAME (max_bits_rsqrt));
#endif
}

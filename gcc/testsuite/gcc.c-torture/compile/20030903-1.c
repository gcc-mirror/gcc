/* Derived from PR optimization/11700.  */
/* The compiler used to ICE during reload for m68k targets.  */

void check_complex (__complex__ double, __complex__ double,
                    __complex__ double, __complex__ int);
void check_float (double, double, double, int);
extern double _Complex conj (double _Complex);
extern double carg (double _Complex __z);

static double minus_zero;

void
conj_test (void)
{
  check_complex (conj (({ __complex__ double __retval;
			  __real__ __retval = (0.0);
			  __imag__ __retval = (0.0);
			  __retval; })),
		 ({ __complex__ double __retval;
		    __real__ __retval = (0.0);
		    __imag__ __retval = (minus_zero);
		    __retval; }), 0, 0);
}

void
carg_test (void)
{
  check_float (carg (({ __complex__ double __retval;
			__real__ __retval = (2.0);
			__imag__ __retval = (0);
			__retval; })), 0, 0, 0);
}


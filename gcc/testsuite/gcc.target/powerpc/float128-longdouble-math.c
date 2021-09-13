/* { dg-require-effective-target ppc_float128_hw } */
/* { dg-options "-mdejagnu-cpu=power9 -O2 -mlong-double-128 -Wno-psabi -mabi=ieeelongdouble" } */

/* Test if switching long double to IEEE 128-bit maps all of the math built-in
   function names correctly.  We leave off the \M in matching the calls, so
   power10 will match using bl foo@notoc.  */

#ifdef DO_FUNC
#define BUILTIN1(FUNC, ARG1)             FUNC (ARG1)
#define BUILTIN2(FUNC, ARG1, ARG2)       FUNC (ARG1, ARG2)
#define BUILTIN3(FUNC, ARG1, ARG2, ARG3) FUNC (ARG1, ARG2, ARG3)

#else
#define BUILTIN1(FUNC, ARG1)             __builtin_ ## FUNC (ARG1)
#define BUILTIN2(FUNC, ARG1, ARG2)       __builtin_ ## FUNC (ARG1, ARG2)
#define BUILTIN3(FUNC, ARG1, ARG2, ARG3) __builtin_ ## FUNC (ARG1, ARG2, ARG3)
#endif

/* Built-in functions that returns a long double and take one long double
   argument.  */

void
return_ld_arg_ld (long double *p,
		  long double *q)
{
  /* { dg-final { scan-assembler {\mbl __acoshieee128} } }  */
  *p++ = BUILTIN1 (acoshl, *q++);

  /* { dg-final { scan-assembler {\mbl __acosieee128} } }  */
  *p++ = BUILTIN1 (acosl, *q++);

  /* { dg-final { scan-assembler {\mbl __asinhieee128} } }  */
  *p++ = BUILTIN1 (asinhl, *q++);

  /* { dg-final { scan-assembler {\mbl __asinieee128} } }  */
  *p++ = BUILTIN1 (asinl, *q++);

  /* { dg-final { scan-assembler {\mbl __atanhieee128} } }  */
  *p++ = BUILTIN1 (atanhl, *q++);

  /* { dg-final { scan-assembler {\mbl __atanieee128} } }  */
  *p++ = BUILTIN1 (atanl, *q++);

  /* { dg-final { scan-assembler {\mbl __cbrtieee128} } }  */
  *p++ = BUILTIN1 (cbrtl, *q++);

  /* inline code.  */
  /* { dg-final { scan-assembler {\mxsrqpi +[0-9]+,[0-9]+,[0-9]+,2} } }  */
  *p++ = BUILTIN1 (ceill, *q++);

  /* { dg-final { scan-assembler {\mbl __coshieee128} } }  */
  *p++ = BUILTIN1 (coshl, *q++);

  /* { dg-final { scan-assembler {\mbl __cosieee128} } }  */
  *p++ = BUILTIN1 (cosl, *q++);

  /* { dg-final { scan-assembler {\mbl __erfcieee128} } }  */
  *p++ = BUILTIN1 (erfcl, *q++);

  /* { dg-final { scan-assembler {\mbl __erfieee128} } }  */
  *p++ = BUILTIN1 (erfl, *q++);

  /* { dg-final { scan-assembler {\mbl __exp10ieee128} } }  */
  *p++ = BUILTIN1 (exp10l, *q++);

  /* { dg-final { scan-assembler {\mbl __exp2ieee128} } }  */
  *p++ = BUILTIN1 (exp2l, *q++);

  /* { dg-final { scan-assembler {\mbl __expieee128} } }  */
  *p++ = BUILTIN1 (expl, *q++);

  /* { dg-final { scan-assembler {\mbl __expm1ieee128} } }  */
  *p++ = BUILTIN1 (expm1l, *q++);

  /* inline code.  */
  /* { dg-final { scan-assembler {\mxsabsqp} } }  */
  *p++ = BUILTIN1 (fabsl, *q++);

  /* inline code.  */
  /* { dg-final { scan-assembler {\mxsrqpi +[0-9]+,[0-9]+,[0-9]+,3} } }  */
  *p++ = BUILTIN1 (floorl, *q++);

  /* { dg-final { scan-assembler {\mbl __lgammaieee128} } }  */
  *p++ = BUILTIN1 (gammal, *q++);

  /* { dg-final { scan-assembler {\mbl __j0ieee128} } }  */
  *p++ = BUILTIN1 (j0l, *q++);

  /* { dg-final { scan-assembler {\mbl __j1ieee128} } }  */
  *p++ = BUILTIN1 (j1l, *q++);

  /* { dg-final { scan-assembler {\mbl __log10ieee128} } }  */
  *p++ = BUILTIN1 (log10l, *q++);

  /* { dg-final { scan-assembler {\mbl __log1pieee128} } }  */
  *p++ = BUILTIN1 (log1pl, *q++);

  /* { dg-final { scan-assembler {\mbl __log2ieee128} } }  */
  *p++ = BUILTIN1 (log2l, *q++);

  /* { dg-final { scan-assembler {\mbl __logbieee128} } }  */
  *p++ = BUILTIN1 (logbl, *q++);

  /* { dg-final { scan-assembler {\mbl __logieee128} } }  */
  *p++ = BUILTIN1 (logl, *q++);

  /* { dg-final { scan-assembler {\mbl __nearbyintieee128} } }  */
  *p++ = BUILTIN1 (nearbyintl, *q++);

  /* { dg-final { scan-assembler {\mbl __exp10ieee128} } }  */
  *p++ = BUILTIN1 (pow10l, *q++);

  /* { dg-final { scan-assembler {\mbl __rintieee128} } }  */
  *p++ = BUILTIN1 (rintl, *q++);

  /* { dg-final { scan-assembler {\mbl __roundevenieee128} } }  */
  *p++ = BUILTIN1 (roundevenl, *q++);

  /* inline code.  */
  /* { dg-final { scan-assembler {\mxsrqpi +[0-9]+,[0-9]+,[0-9]+,0} } }  */
  *p++ = BUILTIN1 (roundl, *q++);

  /* { dg-final { scan-assembler {\mbl __significandieee128} } }  */
  *p++ = BUILTIN1 (significandl, *q++);

  /* { dg-final { scan-assembler {\mbl __sinhieee128} } }  */
  *p++ = BUILTIN1 (sinhl, *q++);

  /* { dg-final { scan-assembler {\mbl __sinieee128} } }  */
  *p++ = BUILTIN1 (sinl, *q++);

  /* { dg-final { scan-assembler {\mbl __sqrtieee128} } }  */
  *p++ = BUILTIN1 (sqrtl, *q++);

  /* { dg-final { scan-assembler {\mbl __tanhieee128} } }  */
  *p++ = BUILTIN1 (tanhl, *q++);

  /* { dg-final { scan-assembler {\mbl __tanieee128} } }  */
  *p++ = BUILTIN1 (tanl, *q++);

  /* { dg-final { scan-assembler {\mbl __tgammaieee128} } }  */
  *p++ = BUILTIN1 (tgammal, *q++);

  /* inline code.  */
  /* { dg-final { scan-assembler {\mxsrqpi +[0-9]+,[0-9]+,[0-9]+,1} } }  */
  *p++ = BUILTIN1 (truncl, *q++);

  /* { dg-final { scan-assembler {\mbl __y0ieee128} } }  */
  *p++ = BUILTIN1 (y0l, *q++);

  /* { dg-final { scan-assembler {\mbl __y1ieee128} } }  */
  *p   = BUILTIN1 (y1l, *q);

}

/* Built-in functions that returns a long double and take two long double
   arguments.  */

void
return_ld_arg_ld_ld (long double *p,
		     long double *q,
		     long double *r)
{
  /* { dg-final { scan-assembler {\mbl __atan2ieee128} } }  */
  *p++ = BUILTIN2 (atan2l, *q++, *r++);

  /* inline code.  */
  /* { dg-final { scan-assembler {\mxscpsgnqp} } }  */
  *p++ = BUILTIN2 (copysignl, *q++, *r++);

  /* { dg-final { scan-assembler {\mbl __remainderieee128} } }  */
  *p++ = BUILTIN2 (dreml, *q++, *r++);

  /* { dg-final { scan-assembler {\mbl __fdimieee128} } }  */
  *p++ = BUILTIN2 (fdiml, *q++, *r++);

  /* { dg-final { scan-assembler {\mbl __fmaxieee128} } }  */
  *p++ = BUILTIN2 (fmaxl, *q++, *r++);

  /* { dg-final { scan-assembler {\mbl __fminieee128} } }  */
  *p++ = BUILTIN2 (fminl, *q++, *r++);

  /* { dg-final { scan-assembler {\mbl __fmodieee128} } }  */
  *p++ = BUILTIN2 (fmodl, *q++, *r++);

  /* { dg-final { scan-assembler {\mbl __hypotieee128} } }  */
  *p++ = BUILTIN2 (hypotl, *q++, *r++);

  /* { dg-final { scan-assembler {\mbl __nextafterieee128} } }  */
  *p++ = BUILTIN2 (nextafterl, *q++, *r++);

  /* { dg-final { scan-assembler {\mbl __nexttowardieee128} } }  */
  *p++ = BUILTIN2 (nexttowardl, *q++, *r++);

  /* { dg-final { scan-assembler {\mbl __powieee128} } }  */
  *p++ = BUILTIN2 (powl, *q++, *r++);

  /* { dg-final { scan-assembler {\mbl __scalbnieee128} } }  */
  *p   = BUILTIN2 (scalbl, *q, *r);
}

/* Built-in function that returns a long double and take three long double
   arguments.  */

void
return_ld_arg_ld_ld_ld (long double *p,
			long double *q,
			long double *r,
			long double *s)
{
  /* inline code.  */
  /* { dg-final { scan-assembler {\mxsmaddqp} } }  */
  *p = BUILTIN3 (fmal, *q, *r, *s);
}

/* Built-in functions that returns a long double and take one
   _Complex long double argument.  */

void
return_ld_arg_cld (long double *p,
		   _Complex long double *q)
{
  /* { dg-final { scan-assembler {\mbl __cabsieee128} } }  */
  *p++ = BUILTIN1 (cabsl, *q++);
}

/* Built-in functions that returns a _Complex long double and takes one
   _Complex long double argument.  */

void
return_cld_arg_cld (_Complex long double *p,
		    _Complex long double *q)
{
  /* { dg-final { scan-assembler {\mbl __cacoshieee128} } }  */
  *p++ = BUILTIN1 (cacoshl, *q++);

  /* { dg-final { scan-assembler {\mbl __cacosieee128} } }  */
  *p++ = BUILTIN1 (cacosl, *q++);

  /* { dg-final { scan-assembler {\mbl __casinhieee128} } }  */
  *p++ = BUILTIN1 (casinhl, *q++);

  /* { dg-final { scan-assembler {\mbl __casinieee128} } }  */
  *p++ = BUILTIN1 (casinl, *q++);

  /* { dg-final { scan-assembler {\mbl __catanhieee128} } }  */
  *p++ = BUILTIN1 (catanhl, *q++);

  /* { dg-final { scan-assembler {\mbl __catanieee128} } }  */
  *p++ = BUILTIN1 (catanl, *q++);

  /* { dg-final { scan-assembler {\mbl __ccoshieee128} } }  */
  *p++ = BUILTIN1 (ccoshl, *q++);

  /* { dg-final { scan-assembler {\mbl __ccosieee128} } }  */
  *p++ = BUILTIN1 (ccosl, *q++);

  /* { dg-final { scan-assembler {\mbl __cexpieee128} } }  */
  *p++ = BUILTIN1 (cexpl, *q++);

  /* { dg-final { scan-assembler {\mbl __clogieee128} } }  */
  *p++ = BUILTIN1 (clogl, *q++);

  /* { dg-final { scan-assembler {\mbl __clog10ieee128} } }  */
  *p++ = BUILTIN1 (clog10l, *q++);

  /* { dg-final { scan-assembler {\mbl __cprojieee128} } }  */
  *p++ = BUILTIN1 (cprojl, *q++);

  /* { dg-final { scan-assembler {\mbl __csinhieee128} } }  */
  *p++ = BUILTIN1 (csinhl, *q++);

  /* { dg-final { scan-assembler {\mbl __csinieee128} } }  */
  *p++ = BUILTIN1 (csinl, *q++);

  /* { dg-final { scan-assembler {\mbl __csqrtieee128} } }  */
  *p++ = BUILTIN1 (csqrtl, *q++);

  /* { dg-final { scan-assembler {\mbl __ctanhieee128} } }  */
  *p++ = BUILTIN1 (ctanhl, *q++);

  /* { dg-final { scan-assembler {\mbl __ctanieee128} } }  */
  *p   = BUILTIN1 (ctanl, *q);
}

/* Built-in functions that returns a _Complex long double and takes one
   long double argument.  */

void
return_cld_arg_ld (_Complex long double *p,
		   long double *q)
{
  /* { dg-final { scan-assembler {\mbl __sincosieee128} } }  */
  *p = BUILTIN1 (cexpil, *q);
}

/* Built-in function that returns a _Complex long double and takes two
   _Complex long double arguments.  */

void
return_cld_arg_cld_cld (_Complex long double *p,
			_Complex long double *q,
			_Complex long double *r)
{
  /* { dg-final { scan-assembler {\mbl __cpowieee128} } }  */
  *p = BUILTIN2 (cpowl, *q, *r);
}

/* Built-in functions that returns a long double and takes a long double and a
   pointer to an int arguments.  */

void
return_ld_arg_ld_pi (long double *p,
		     long double *q,
		     int **r)
{
  /* { dg-final { scan-assembler {\mbl __frexpieee128} } }  */
  *p++ = BUILTIN2 (frexpl, *q++, *r++);

  /* { dg-final { scan-assembler {\mbl __lgammaieee128_r} } }  */
  *p++ = BUILTIN2 (gammal_r, *q++, *r++);
}

/* Built-in functions that returns a long double and takes a long double and an
   int arguments.  */

void
return_ld_arg_ld_i (long double *p,
		    long double *q,
		    int *r)
{
  /* { dg-final { scan-assembler {\mbl __ldexpieee128} } }  */
  *p++ = BUILTIN2 (ldexpl, *q++, *r++);

  /* { dg-final { scan-assembler {\mbl __powikf2} } }  */
  *p++ = BUILTIN2 (powil, *q++, *r++);

  /* { dg-final { scan-assembler {\mbl __scalbnieee128} } }  */
  *p   = BUILTIN2 (scalbnl, *q, *r);
}

/* Built-in function that returns a long double and takes a long double and a
   long arguments.  */

void
return_ld_arg_ld_l (long double *p,
		    long double *q,
		    long *r)
{
  /* { dg-final { scan-assembler {\mbl __scalblnieee128} } }  */
  *p = BUILTIN2 (scalblnl, *q, *r);
}

/* Built-in functions that returns a long double and takes a long double and a
   long long arguments.  */

void
return_ld_arg_i_ld (long double *p,
		    int *q,
		    long double *r)
{
  /* { dg-final { scan-assembler {\mbl __jnieee128} } }  */
  *p++ = BUILTIN2 (jnl, *q++, *r++);

  /* { dg-final { scan-assembler {\mbl __ynieee128} } }  */
  *p   = BUILTIN2 (ynl, *q, *r);
}

/* Built-in functions that returns a long double and takes a long double and a
   pointer to a long double arguments.  */

void
return_ld_arg_ld_pld (long double *p,
		      long double *q,
		      long double **r)
{
  /* { dg-final { scan-assembler {\mbl __modfieee128} } }  */
  *p = BUILTIN2 (modfl, *q, *r);
}

/* Built-in function that returns a long double and takes two long double and a
   pointer to an int arguments.  */

void
return_ld_arg_ld_ld_pi (long double *p,
			long double *q,
			long double *r,
			int **s)
{
  /* { dg-final { scan-assembler {\mbl __remquoieee128} } }  */
  *p = BUILTIN3 (remquol, *q, *r, *s);
}

/* Built-in functions that return san int and takes one long double argument.  */

void
return_i_arg_ld (int *p,
		 long double *q)
{
  /* { dg-final { scan-assembler {\mbl __ceilieee128} } }  */
  *p++ = BUILTIN1 (iceill, *q++);

  /* { dg-final { scan-assembler {\mbl __floorieee128} } }  */
  *p++ = BUILTIN1 (ifloorl, *q++);

  /* { dg-final { scan-assembler {\mbl __ilogbieee128} } }  */
  *p++ = BUILTIN1 (ilogbl, *q++);

  /* { dg-final { scan-assembler {\mbl __lrintieee128} } }  */
  *p++ = BUILTIN1 (irintl, *q++);

  /* { dg-final { scan-assembler {\mbl __lroundieee128} } }  */
  *p++ = BUILTIN1 (iroundl, *q++);

  /* inline code.  */
  /* { dg-final { scan-assembler {\mxscvqpswz} } }  */
  *p++ = BUILTIN1 (signbitl, *q++);
}

/* Built-in function that returns a double and takes one double and one long
   double arguments.  */

void
return_d_arg_d_ld (double *p,
		   double *q,
		   long double *r)
{
  /* { dg-final { scan-assembler {\mbl __nexttoward_to_ieee128} } }  */
  *p = BUILTIN2 (nexttoward, *q, *r);
}

/* Built-in function that returns a float and takes one float and one long
   double arguments.  */

void
return_f_arg_f_ld (float *p,
		   float *q,
		   long double *r)
{
  /* { dg-final { scan-assembler {\mbl __nexttowardf_to_ieee128} } }  */
  *p = BUILTIN2 (nexttowardf, *q, *r);
}

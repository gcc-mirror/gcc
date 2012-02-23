/* Test for printf formats for Decimal Floating Point types.  */

/* { dg-do compile } */
/* { dg-require-effective-target dfp } */
/* { dg-options "-Wformat" } */
/* { dg-skip-if "No scanf/printf dfp support" { *-*-mingw* } { "*" } { "" } } */

extern int printf (const char *restrict, ...);

void
foo (_Decimal32 x, _Decimal64 y, _Decimal128 z, int i, unsigned int j,
     double d, char *p)
{
  /* See ISO/IEC DTR 24732 subclause 9.3 (currently Working Draft 5 from
      2005-03-06).  */
  /* Formatted input/output specifiers.  */

  /* Check lack of warnings for valid usage.  */

  printf ("%Hf\n", x);
  printf ("%HF\n", x);
  printf ("%He\n", x);
  printf ("%HE\n", x);
  printf ("%Hg\n", x);
  printf ("%HG\n", x);

  printf ("%Df\n", y);
  printf ("%DF\n", y);
  printf ("%De\n", y);
  printf ("%DE\n", y);
  printf ("%Dg\n", y);
  printf ("%DG\n", y);

  printf ("%DDf\n", z);
  printf ("%DDF\n", z);
  printf ("%DDe\n", z);
  printf ("%DDE\n", z);
  printf ("%DDg\n", z);
  printf ("%DDG\n", z);

  printf ("%DG%DDE%HF%DDe%He%HE%DF%DDF%De%DDG%HG%Df%Hg%DE%DDf%Dg%DDg%Hf\n",
           y, z, x, z, x, x, y, z, y, z, x, y, x, y, z, y, z, x);

  /* Check warnings for type mismatches.  */

  printf ("%Hf\n", y);	/* { dg-warning "expects argument" "bad use of %H" } */
  printf ("%HF\n", y);	/* { dg-warning "expects argument" "bad use of %H" } */
  printf ("%He\n", y);	/* { dg-warning "expects argument" "bad use of %H" } */
  printf ("%HE\n", y);	/* { dg-warning "expects argument" "bad use of %H" } */
  printf ("%Hg\n", y);	/* { dg-warning "expects argument" "bad use of %H" } */
  printf ("%HG\n", y);	/* { dg-warning "expects argument" "bad use of %H" } */
  printf ("%Hf\n", z);	/* { dg-warning "expects argument" "bad use of %H" } */
  printf ("%HF\n", z);	/* { dg-warning "expects argument" "bad use of %H" } */
  printf ("%He\n", z);	/* { dg-warning "expects argument" "bad use of %H" } */
  printf ("%HE\n", z);	/* { dg-warning "expects argument" "bad use of %H" } */
  printf ("%Hg\n", z);	/* { dg-warning "expects argument" "bad use of %H" } */
  printf ("%HG\n", z);	/* { dg-warning "expects argument" "bad use of %H" } */

  printf ("%Df\n", x);	/* { dg-warning "expects argument" "bad use of %D" } */
  printf ("%DF\n", x);	/* { dg-warning "expects argument" "bad use of %D" } */
  printf ("%De\n", x);	/* { dg-warning "expects argument" "bad use of %D" } */
  printf ("%DE\n", x);	/* { dg-warning "expects argument" "bad use of %D" } */
  printf ("%Dg\n", x);	/* { dg-warning "expects argument" "bad use of %D" } */
  printf ("%DG\n", x);	/* { dg-warning "expects argument" "bad use of %D" } */
  printf ("%Df\n", z);	/* { dg-warning "expects argument" "bad use of %D" } */
  printf ("%DF\n", z);	/* { dg-warning "expects argument" "bad use of %D" } */
  printf ("%De\n", z);	/* { dg-warning "expects argument" "bad use of %D" } */
  printf ("%DE\n", z);	/* { dg-warning "expects argument" "bad use of %D" } */
  printf ("%Dg\n", z);	/* { dg-warning "expects argument" "bad use of %D" } */
  printf ("%DG\n", z);	/* { dg-warning "expects argument" "bad use of %D" } */

  printf ("%DDf\n", x);	/* { dg-warning "expects argument" "bad use of %DD" } */
  printf ("%DDF\n", x);	/* { dg-warning "expects argument" "bad use of %DD" } */
  printf ("%DDe\n", x);	/* { dg-warning "expects argument" "bad use of %DD" } */
  printf ("%DDE\n", x);	/* { dg-warning "expects argument" "bad use of %DD" } */
  printf ("%DDg\n", x);	/* { dg-warning "expects argument" "bad use of %DD" } */
  printf ("%DDG\n", x);	/* { dg-warning "expects argument" "bad use of %DD" } */
  printf ("%DDf\n", y);	/* { dg-warning "expects argument" "bad use of %DD" } */
  printf ("%DDF\n", y);	/* { dg-warning "expects argument" "bad use of %DD" } */
  printf ("%DDe\n", y);	/* { dg-warning "expects argument" "bad use of %DD" } */
  printf ("%DDE\n", y);	/* { dg-warning "expects argument" "bad use of %DD" } */
  printf ("%DDg\n", y);	/* { dg-warning "expects argument" "bad use of %DD" } */
  printf ("%DDG\n", y);	/* { dg-warning "expects argument" "bad use of %DD" } */

  /* Check for warnings for bad use of H, D, and DD length specifiers.  */

  printf ("%Hd\n", i);	/* { dg-warning "length" "bad use of %H" } */
  printf ("%Hi\n", i);	/* { dg-warning "length" "bad use of %H" } */
  printf ("%Ho\n", j);	/* { dg-warning "length" "bad use of %H" } */
  printf ("%Hu\n", j);	/* { dg-warning "length" "bad use of %H" } */
  printf ("%Hx\n", j);	/* { dg-warning "length" "bad use of %H" } */
  printf ("%HX\n", j);	/* { dg-warning "length" "bad use of %H" } */
  printf ("%Ha\n", d);	/* { dg-warning "length" "bad use of %H" } */
  printf ("%HA\n", d);	/* { dg-warning "length" "bad use of %H" } */
  printf ("%Hc\n", i);	/* { dg-warning "length" "bad use of %H" } */
  printf ("%Hs\n", p);	/* { dg-warning "length" "bad use of %H" } */
  printf ("%Hp\n", p);	/* { dg-warning "length" "bad use of %H" } */
  printf ("%Hn\n", p);	/* { dg-warning "length" "bad use of %H" } */

  /* Sanity checks for flags, field width, and precision in formats for
     DFP types.  */

  printf ("%-Hf\n", x);
  printf ("%+HF\n", x);
  printf ("% He\n", x);
  printf ("%#HE\n", x);
  printf ("%0Hg\n", x);
  printf ("%#0HG\n", x);

  printf ("%0#Df\n", y);
  printf ("%0DF\n", y);
  printf ("%#De\n", y);
  printf ("%-#DE\n", y);
  printf ("%-#0Dg\n", y);  /* { dg-warning "flag ignored" "ignore flag" } */
  printf ("%0+ DG\n", y);  /* { dg-warning "flag ignored" "ignore flag" } */

  printf ("%DDf\n", z);
  printf ("%0DDF\n", z);
  printf ("%#0DDe\n", z);
  printf ("%+DDE\n", z);
  printf ("%0-#DDg\n", z); /* { dg-warning "flag ignored" "ignore flag" } */
  printf ("% DDG\n", z);
}

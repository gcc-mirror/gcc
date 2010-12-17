/* Test for scanf formats for Decimal Floating Point types.  */

/* { dg-do compile } */
/* { dg-require-effective-target dfp } */
/* { dg-options "-Wformat" } */
/* { dg-skip-if "No scanf/printf dfp support" { *-*-mingw* } } */

#include "format.h"

void
voo (_Decimal32 *x, _Decimal64 *y, _Decimal128 *z, int *i, unsigned int *j,
     double *d, char **p)
{
  /* See ISO/IEC DTR 24732 subclause 9.3 (currently Working Draft 5 from
      2005-03-06).  */
  /* Formatted input/output specifiers.  */

  /* Check lack of warnings for valid usage.  */

  scanf ("%Hf", x);
  scanf ("%HF", x);
  scanf ("%He", x);
  scanf ("%HE", x);
  scanf ("%Hg", x);
  scanf ("%HG", x);

  scanf ("%Df", y);
  scanf ("%DF", y);
  scanf ("%De", y);
  scanf ("%DE", y);
  scanf ("%Dg", y);
  scanf ("%DG", y);

  scanf ("%DDf", z);
  scanf ("%DDF", z);
  scanf ("%DDe", z);
  scanf ("%DDE", z);
  scanf ("%DDg", z);
  scanf ("%DDG", z);

  scanf ("%DG%DDE%HF%DDe%He%HE%DF%DDF%De%DDG%HG%Df%Hg%DE%DDf%Dg%DDg%Hf\n",
           y, z, x, z, x, x, y, z, y, z, x, y, x, y, z, y, z, x);

  /* Check warnings for type mismatches.  */

  scanf ("%Hf", y);	/* { dg-warning "expects argument" "bad use of %H" } */
  scanf ("%HF", y);	/* { dg-warning "expects argument" "bad use of %H" } */
  scanf ("%He", y);	/* { dg-warning "expects argument" "bad use of %H" } */
  scanf ("%HE", y);	/* { dg-warning "expects argument" "bad use of %H" } */
  scanf ("%Hg", y);	/* { dg-warning "expects argument" "bad use of %H" } */
  scanf ("%HG", y);	/* { dg-warning "expects argument" "bad use of %H" } */
  scanf ("%Hf", z);	/* { dg-warning "expects argument" "bad use of %H" } */
  scanf ("%HF", z);	/* { dg-warning "expects argument" "bad use of %H" } */
  scanf ("%He", z);	/* { dg-warning "expects argument" "bad use of %H" } */
  scanf ("%HE", z);	/* { dg-warning "expects argument" "bad use of %H" } */
  scanf ("%Hg", z);	/* { dg-warning "expects argument" "bad use of %H" } */
  scanf ("%HG", z);	/* { dg-warning "expects argument" "bad use of %H" } */

  scanf ("%Df", x);	/* { dg-warning "expects argument" "bad use of %D" } */
  scanf ("%DF", x);	/* { dg-warning "expects argument" "bad use of %D" } */
  scanf ("%De", x);	/* { dg-warning "expects argument" "bad use of %D" } */
  scanf ("%DE", x);	/* { dg-warning "expects argument" "bad use of %D" } */
  scanf ("%Dg", x);	/* { dg-warning "expects argument" "bad use of %D" } */
  scanf ("%DG", x);	/* { dg-warning "expects argument" "bad use of %D" } */
  scanf ("%Df", z);	/* { dg-warning "expects argument" "bad use of %D" } */
  scanf ("%DF", z);	/* { dg-warning "expects argument" "bad use of %D" } */
  scanf ("%De", z);	/* { dg-warning "expects argument" "bad use of %D" } */
  scanf ("%DE", z);	/* { dg-warning "expects argument" "bad use of %D" } */
  scanf ("%Dg", z);	/* { dg-warning "expects argument" "bad use of %D" } */
  scanf ("%DG", z);	/* { dg-warning "expects argument" "bad use of %D" } */

  scanf ("%DDf", x);	/* { dg-warning "expects argument" "bad use of %DD" } */
  scanf ("%DDF", x);	/* { dg-warning "expects argument" "bad use of %DD" } */
  scanf ("%DDe", x);	/* { dg-warning "expects argument" "bad use of %DD" } */
  scanf ("%DDE", x);	/* { dg-warning "expects argument" "bad use of %DD" } */
  scanf ("%DDg", x);	/* { dg-warning "expects argument" "bad use of %DD" } */
  scanf ("%DDG", x);	/* { dg-warning "expects argument" "bad use of %DD" } */
  scanf ("%DDf", y);	/* { dg-warning "expects argument" "bad use of %DD" } */
  scanf ("%DDF", y);	/* { dg-warning "expects argument" "bad use of %DD" } */
  scanf ("%DDe", y);	/* { dg-warning "expects argument" "bad use of %DD" } */
  scanf ("%DDE", y);	/* { dg-warning "expects argument" "bad use of %DD" } */
  scanf ("%DDg", y);	/* { dg-warning "expects argument" "bad use of %DD" } */
  scanf ("%DDG", y);	/* { dg-warning "expects argument" "bad use of %DD" } */

  /* Check for warnings for bad use of H, D, and DD length specifiers.  */

  scanf ("%Hd\n", i);	/* { dg-warning "length" "bad use of %H" } */
  scanf ("%Hi\n", i);	/* { dg-warning "length" "bad use of %H" } */
  scanf ("%Ho\n", j);	/* { dg-warning "length" "bad use of %H" } */
  scanf ("%Hu\n", j);	/* { dg-warning "length" "bad use of %H" } */
  scanf ("%Hx\n", j);	/* { dg-warning "length" "bad use of %H" } */
  scanf ("%HX\n", j);	/* { dg-warning "length" "bad use of %H" } */
  scanf ("%Ha\n", d);	/* { dg-warning "length" "bad use of %H" } */
  scanf ("%HA\n", d);	/* { dg-warning "length" "bad use of %H" } */
  scanf ("%Hc\n", i);	/* { dg-warning "length" "bad use of %H" } */
  scanf ("%Hs\n", p);	/* { dg-warning "length" "bad use of %H" } */
  scanf ("%Hp\n", p);	/* { dg-warning "length" "bad use of %H" } */
  scanf ("%Hn\n", p);	/* { dg-warning "length" "bad use of %H" } */
}

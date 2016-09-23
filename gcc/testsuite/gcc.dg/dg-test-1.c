/* Test relative line number specification extensions over what DejaGNU supports.  */
/* { dg-do compile } */
/* { dg-options "-Wunused-parameter" } */

void
foo (void)
{			/* { dg-error "'a' undeclared" "err1" { target *-*-* } .+1 } */
  int z = a + b + c + d;/* { dg-error "'b' undeclared" "err2" { target *-*-* } . } */
}			/* { dg-error "'c' undeclared" "err3" { target *-*-* } .-1 } */


/* { dg-error "'d' undeclared" "err4" { target *-*-* } .-4 } */
/* { dg-warning "unused parameter 'e'" "warn1" { target *-*-* } .+3 } */

void				/* { dg-warning "unused parameter 'f'" "warn2" { target *-*-* } .+1 } */
bar (int e, int f, int g, int h)/* { dg-warning "unused parameter 'g'" "warn3" { target *-*-* } . } */
{				/* { dg-warning "unused parameter 'h'" "warn4" { target *-*-* } .-1 } */
}

/* Test relative line number specification extensions over what DejaGNU supports.  */
/* { dg-do compile } */
/* { dg-options "-Wunused-parameter" } */

void
foo (void)
{			/* { dg-error "'a' undeclared" "err1" { target *-*-* } .+1 } */
  int z = a + b + c + d;/* { dg-error "'b' undeclared" "err2" } */
}			/* { dg-error "'c' undeclared" "err3" { target *-*-* } .-1 } */


/* { dg-error "'d' undeclared" "err4" { target *-*-* } .-4 } */
/* { dg-warning "unused parameter 'e'" "warn1" { target *-*-* } .+3 } */

void				/* { dg-warning "unused parameter 'f'" "warn2" { target *-*-* } .+1 } */
bar (int e, int f, int g, int h)/* { dg-warning "unused parameter 'g'" "warn3" } */
{				/* { dg-warning "unused parameter 'h'" "warn4" { target *-*-* } .-1 } */
}


/* Ensure that relative line numbers with more than one digit are supported.  */
/* { dg-warning "unused parameter 'i'" "warn5" { target *-*-* } .+10 } */








void
baz (int i, int j)
{
}







/* { dg-warning "unused parameter 'j'" "warn6" { target *-*-* } .-10 } */

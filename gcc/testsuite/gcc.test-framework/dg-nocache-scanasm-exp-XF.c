/* { dg-do compile } */
/* { dg-options "-DDEFINED" } */

int
foo ()
{
  return 0;
}

/* A few examples from scanasm.exp.  */

/* { dg-final { scan-assembler "whatever" { xfail def_nocache } } } */
/* { dg-final { scan-assembler-not "foo" { xfail def_nocache } } } */
/* { dg-final { scan-hidden "whatever" { xfail def_nocache } } } */

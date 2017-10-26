/* { dg-do compile } */
/* { dg-options "-fcilkplus -Wunknown-pragmas" } */
/* { dg-additional-options "-std=c99" { target c } } */


char Array1[26];

#pragma cilk grainsize = 2 /* { dg-error "must be inside a function" } */

int main(int argc, char **argv)
{
/* This is OK.  */
#pragma cilk grainsize = 2
  _Cilk_for (int ii = 0; ii < 10; ii++)
    Array1[ii] = 0;

#pragma cilk grainsize 2 /* { dg-error "expected '=' before numeric constant" } */
  _Cilk_for (int ii = 0; ii < 10; ii++)
    Array1[ii] = 0;

#pragma cilk grainsiz = 2 /* { dg-warning "-:ignoring #pragma cilk grainsiz" } */
  _Cilk_for (int ii = 0; ii < 10; ii++)
    Array1[ii] = 0;


/* This is OK, it will do a type conversion to long int.  */
#pragma cilk grainsize = 0.5
  _Cilk_for (int ii = 0; ii < 10; ii++)
    Array1[ii] = 0;

#pragma cilk grainsize = 1
  while (Array1[5] != 0) /* { dg-warning "is not followed by" } */
    {
    /* Blah */
    }

#pragma cilk grainsize = 1
  int q = 0; /* { dg-warning "is not followed by" } */
  _Cilk_for (q = 0; q < 10; q++) /* { dg-error "allows expression instead of declaration" "" { target c++ } } */
    Array1[q] = 5;

  while (Array1[5] != 0)
    {
    /* Blah */
    }

  return 0;
}

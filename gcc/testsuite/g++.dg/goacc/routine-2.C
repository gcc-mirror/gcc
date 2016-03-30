/* Test invalid use of the routine directive.  */

template <typename T>
extern T one_d();
#pragma acc routine (one_d) /* { dg-error "names a set of overloads" } */

template <typename T>
T
one()
{
  return 1;
}
#pragma acc routine (one) /* { dg-error "names a set of overloads" } */

int incr (int);
float incr (float);
int inc;

#pragma acc routine (incr) /* { dg-error "names a set of overloads" } */

#pragma acc routine (increment) /* { dg-error "has not been declared" } */

#pragma acc routine (inc) /* { dg-error "does not refer to a function" } */

#pragma acc routine (+) /* { dg-error "expected unqualified-id before '.' token" } */

int sum (int, int);

namespace foo {
#pragma acc routine (sum)
  int sub (int, int);
}

#pragma acc routine (foo::sub)

/* It's strange to apply a routine directive to subset of overloaded
   functions, but that is permissible in OpenACC 2.x.  */

int decr (int a);

#pragma acc routine
float decr (float a);

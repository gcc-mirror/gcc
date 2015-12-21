// { dg-options "-fdiagnostics-show-caret" }

// Adapted from https://gcc.gnu.org/wiki/ClangDiagnosticsComparison

typedef float __m128;
void test_1 ()
{
  __m128 myvec[2];
  int const *ptr;
  myvec[1] / ptr; // { dg-error "invalid operands" }

/* { dg-begin-multiline-output "" }
   myvec[1] / ptr;
   ~~~~~~~~~^~~~~
   { dg-end-multiline-output "" } */
}

struct s {};
struct t {};
extern struct s some_function (void);
extern struct t some_other_function (void);

int test_2 (void)
{
  return (some_function ()
	  + some_other_function ()); // { dg-error "no match for .operator" }

/* { dg-begin-multiline-output "" }
   return (some_function ()
           ~~~~~~~~~~~~~~~~
    + some_other_function ());
    ^~~~~~~~~~~~~~~~~~~~~~~~
   { dg-end-multiline-output "" } */
}

int test_3 (struct s param_s, struct t param_t)
{
  return param_s && param_t; // { dg-error "no match for .operator" }

/* { dg-begin-multiline-output "" }
   return param_s && param_t;
          ~~~~~~~~^~~~~~~~~~
   { dg-end-multiline-output "" } */
}

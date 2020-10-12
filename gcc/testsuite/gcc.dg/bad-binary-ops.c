/* { dg-options "-fdiagnostics-show-caret" } */

/* Adapted from https://gcc.gnu.org/wiki/ClangDiagnosticsComparison */

typedef float __m128;
void test_1 ()
{
  __m128 myvec[2];
  int const *ptr;
  myvec[1]/ptr; /* { dg-error "invalid operands to binary /" } */

/* TODO: ideally we'd underline "ptr" as well.
{ dg-begin-multiline-output "" }
   myvec[1]/ptr;
   ~~~~~~~~^
        |
        __m128
{ dg-end-multiline-output "" } */


}

struct s {};
struct t {};
extern struct s some_function (void);
extern struct t some_other_function (void);

int test_2 (void)
{
  return (some_function ()
	  + some_other_function ()); /* { dg-error "invalid operands to binary \+" } */

/* { dg-begin-multiline-output "" }
   return (some_function ()
           ~~~~~~~~~~~~~~~~
           |
           struct s
           + some_other_function ());
           ^ ~~~~~~~~~~~~~~~~~~~~~~
             |
             struct t
   { dg-end-multiline-output "" } */
}

int test_3 (struct s param_s, struct t param_t)
{
  return param_s + param_t; // { dg-error "invalid operands to binary \+" }

/* { dg-begin-multiline-output "" }
   return param_s + param_t;
                  ^
   { dg-end-multiline-output "" } */
/* TODO: ideally we'd underline both params here.  */
}

typedef struct s S;
typedef struct t T;

extern S callee_4a (void);
extern T callee_4b (void);

int test_4 (void)
{
  return callee_4a () + callee_4b (); /* { dg-error "invalid operands to binary \+" } */

/* { dg-begin-multiline-output "" }
   return callee_4a () + callee_4b ();
          ~~~~~~~~~~~~ ^ ~~~~~~~~~~~~
          |              |
          |              T {aka struct t}
          S {aka struct s}
   { dg-end-multiline-output "" } */
}


/* PR c/71858 */
/* Only consider function names, function pointers and macros for implicit function declarations.  */
/* { dg-do compile } */
/* { dg-options "-Wimplicit-function-declaration -fdiagnostics-show-caret" } */

void fn1abcd (void);

void
test_1 (int fn1bacd)
{
  fn1badc (); /* { dg-warning "3: implicit declaration of function .fn1badc.; did you mean .fn1abcd.?" } */
  /* { dg-begin-multiline-output "" }
   fn1badc ();
   ^~~~~~~
   fn1abcd
   { dg-end-multiline-output "" } */
}

void fn2efgh (void);
void (*fn2efhg) (void);

void
test_2 (void)
{
  fn2fehg (); /* { dg-warning "3: implicit declaration of function .fn2fehg.; did you mean .fn2efhg.?" } */
  /* { dg-begin-multiline-output "" }
   fn2fehg ();
   ^~~~~~~
   fn2efhg
   { dg-end-multiline-output "" } */
}

void fn3ijkl (void);
typedef int fn3ijlk;

void
test_3 (void)
{
  fn3jilk (); /* { dg-warning "3: implicit declaration of function .fn3jilk.; did you mean .fn3ijkl.?" } */
  /* { dg-begin-multiline-output "" }
   fn3jilk ();
   ^~~~~~~
   fn3ijkl
   { dg-end-multiline-output "" } */
}

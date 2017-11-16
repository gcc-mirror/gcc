/* Test for warning of and recovery from recursion in function-like
   macros.  */

/* { dg-do preprocess } */

#define foo() foo()
foo();				/* { dg-error "-:detected recursion" } */

#define bar() bar baz() bar
bar();				/* { dg-bogus "detected recursion" } */

#define baz() foo()
baz();			       /* { dg-error "-:detected recursion" } */

#define a(x) x(a)
a(a);			       /* { dg-error "-:detected recursion" } */

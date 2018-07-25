/* Miscellaneous OpenACC routine front end checking.  */

/* Pragma context.  */

struct PC
{
#pragma acc routine seq /* { dg-error ".#pragma acc routine. must be at file scope" } */
};

void PC1( /* { dg-bogus "variable or field .PC1. declared void" "TODO" { xfail c++ } } */
#pragma acc routine seq
	 /* { dg-error ".#pragma acc routine. must be at file scope" "" { target c } .-1 }
	    { dg-error ".#pragma. is not allowed here" "" { target c++ } .-2 } */
) /* { dg-bogus "expected declaration specifiers or .\\.\\.\\.. before .\\). token" "TODO" { xfail c } } */
{
}

void PC2()
{
  if (0)
#pragma acc routine seq /* { dg-error ".#pragma acc routine. must be at file scope" } */
    ;
}

void PC3()
{
#pragma acc routine seq /* { dg-error ".#pragma acc routine. must be at file scope" } */
}


/* "( name )" syntax.  */

#pragma acc routine ( /* { dg-error "expected (function name|unqualified-id) before end of line" } */
#pragma acc routine () seq /* { dg-error "expected (function name|unqualified-id) before .\\). token" } */
#pragma acc routine (+) seq /* { dg-error "expected (function name|unqualified-id) before .\\+. token" } */
#pragma acc routine (?) seq /* { dg-error "expected (function name|unqualified-id) before .\\?. token" } */
#pragma acc routine (:) seq /* { dg-error "expected (function name|unqualified-id) before .:. token" } */
#pragma acc routine (4) seq /* { dg-error "expected (function name|unqualified-id) before numeric constant" } */
#pragma acc routine ('4') /* { dg-error "expected (function name|unqualified-id) before .4." } */
#pragma acc routine ("4") seq /* { dg-error "expected (function name|unqualified-id) before string constant" } */
extern void R1(void);
extern void R2(void);
#pragma acc routine (R1, R2, R3) worker /* { dg-error "expected .\\). before .,. token" } */
#pragma acc routine (R1 R2 R3) worker /* { dg-error "expected .\\). before .R2." } */
#pragma acc routine (R1) worker
#pragma acc routine (R2) worker


/* "#pragma acc routine" not immediately followed by (a single) function
   declaration or definition.  */

#pragma acc routine seq /* { dg-error ".#pragma acc routine. not immediately followed by function declaration or definition" } */
int a;

#pragma acc routine seq /* { dg-error ".#pragma acc routine. not immediately followed by a single function declaration or definition" } */
void fn1 (void), fn1b (void);

#pragma acc routine seq /* { dg-error ".#pragma acc routine. not immediately followed by function declaration or definition" } */
int b, fn2 (void);

#pragma acc routine seq /* { dg-error ".#pragma acc routine. not immediately followed by function declaration or definition" } */
int b_, fn2_ (void), B_;

#pragma acc routine seq /* { dg-error ".#pragma acc routine. not immediately followed by a single function declaration or definition" } */
int fn3 (void), b2;

#pragma acc routine seq /* { dg-error ".#pragma acc routine. not immediately followed by function declaration or definition" } */
typedef struct c c;

#pragma acc routine seq /* { dg-error ".#pragma acc routine. not immediately followed by function declaration or definition" } */
struct d {} d;

#pragma acc routine seq /* { dg-error ".#pragma acc routine. not immediately followed by function declaration or definition" } */
#pragma acc routine seq /* { dg-error ".#pragma acc routine. not immediately followed by a single function declaration or definition" } */
void fn1_2 (void), fn1b_2 (void);

#pragma acc routine seq /* { dg-error ".#pragma acc routine. not immediately followed by function declaration or definition" } */
#pragma acc routine /* { dg-error ".#pragma acc routine. not immediately followed by function declaration or definition" } */
int b_2, fn2_2 (void);

#pragma acc routine seq /* { dg-error ".#pragma acc routine. not immediately followed by function declaration or definition" } */
#pragma acc routine seq /* { dg-error ".#pragma acc routine. not immediately followed by function declaration or definition" } */
int b_2_, fn2_2_ (void), B_2_;

#pragma acc routine seq /* { dg-error ".#pragma acc routine. not immediately followed by function declaration or definition" } */
#pragma acc routine seq /* { dg-error ".#pragma acc routine. not immediately followed by a single function declaration or definition" } */
int fn3_2 (void), b2_2;

#pragma acc routine seq /* { dg-error ".#pragma acc routine. not immediately followed by function declaration or definition" } */
#pragma acc routine seq /* { dg-error ".#pragma acc routine. not immediately followed by function declaration or definition" } */
typedef struct c_2 c_2;

#pragma acc routine seq /* { dg-error ".#pragma acc routine. not immediately followed by function declaration or definition" } */
#pragma acc routine seq /* { dg-error ".#pragma acc routine. not immediately followed by function declaration or definition" } */
struct d_2 {} d_2;

#pragma acc routine seq /* { dg-error ".#pragma acc routine. not immediately followed by function declaration or definition" } */
#pragma acc routine seq
int fn4 (void);

int fn5a (void);
int fn5b (void);
#pragma acc routine seq /* { dg-error ".#pragma acc routine. not immediately followed by function declaration or definition" } */
#pragma acc routine (fn5a) seq
#pragma acc routine (fn5b) seq
int fn5 (void);

#pragma acc routine seq /* { dg-error ".#pragma acc routine. not immediately followed by function declaration or definition" } */
#pragma acc routine (fn6a) seq /* { dg-error ".fn6a. has not been declared" } */
#pragma acc routine (fn6b) seq /* { dg-error ".fn6b. has not been declared" } */
int fn6 (void);

#ifdef __cplusplus

#pragma acc routine seq /* { dg-error ".#pragma acc routine. not immediately followed by function declaration or definition" "" { target c++ } } */
namespace f {}

namespace g {}

#pragma acc routine seq /* { dg-error ".#pragma acc routine. not immediately followed by function declaration or definition" "" { target c++ } } */
using namespace g;

#pragma acc routine (g) seq /* { dg-error ".g. does not refer to a function" "" { target c++ } } */

#endif /* __cplusplus */

#pragma acc routine (a) seq /* { dg-error ".a. does not refer to a function" } */
  
#pragma acc routine (c) seq /* { dg-error ".c. does not refer to a function" } */


/* Static assert.  */

#pragma acc routine /* { dg-bogus ".#pragma acc routine. not immediately followed by function declaration or definition" "TODO" { xfail *-*-* } } */
#ifndef __cplusplus /* C */
_Static_assert(0, ""); /* { dg-error "static assertion failed" "" { target c } } */
#elif __cplusplus < 201103L /* C++98 */
/* C++98 doesn't support static_assert, so fake an error in combination, and as
   expected with the "#pragma acc routine" above.  */
int dummy_instead_of_static_assert;
#else /* C++ */
static_assert(0, ""); /* { dg-error "static assertion failed" "" { target c++11 } } */
#endif
void f_static_assert();
/* Check that we already recognized "f_static_assert" as an OpenACC routine.  */
#pragma acc routine (f_static_assert) seq /* { dg-error ".#pragma acc routine. already applied to .\[void \]*f_static_assert" "TODO" { xfail *-*-* } } */


/* __extension__ usage.  */

#pragma acc routine seq
__extension__ extern void ex1();
#pragma acc routine (ex1) /* { dg-error ".#pragma acc routine. already applied to .\[void \]*ex1" } */

#pragma acc routine seq
__extension__ __extension__ __extension__ __extension__ __extension__ void ex2()
{
}
#pragma acc routine (ex2) /* { dg-error ".#pragma acc routine. already applied to .\[void \]*ex2" } */

#pragma acc routine seq /* { dg-error ".#pragma acc routine. not immediately followed by function declaration or definition" } */
__extension__ int ex3;
#pragma acc routine (ex3) seq /* { dg-error ".ex3. does not refer to a function" } */


/* "#pragma acc routine" must be applied before.  */

void Bar ();

void Foo ()
{
  Bar ();
}

#pragma acc routine (Bar) seq // { dg-error ".#pragma acc routine. must be applied before use" }

#pragma acc routine (Foo) gang // { dg-error ".#pragma acc routine. must be applied before definition" }

#pragma acc routine (Baz) seq // { dg-error "not been declared" }


/* OpenACC declare.  */

int vb1;		/* { dg-error "directive for use" } */
extern int vb2;		/* { dg-error "directive for use" } */
static int vb3;		/* { dg-error "directive for use" } */

#pragma acc routine seq
int
func1 (int a)
{
  vb1 = a + 1;
  vb2 = vb1 + 1;
  vb3 = vb2 + 1;

  return vb3;
}

#pragma acc routine seq
int
func2 (int a)
{
  extern int vb4;	/* { dg-error "directive for use" } */
  static int vb5;	/* { dg-error "directive for use" } */

  vb4 = a + 1;
  vb5 = vb4 + 1;

  return vb5;
}

extern int vb6;			/* { dg-error "clause used in" } */
#pragma acc declare link (vb6)
static int vb7;			/* { dg-error "clause used in" } */
#pragma acc declare link (vb7)

#pragma acc routine seq
int
func3 (int a)
{
  vb6 = a + 1;
  vb7 = vb6 + 1;

  return vb7;
}

int vb8;
#pragma acc declare create (vb8)
extern int vb9;
#pragma acc declare create (vb9)
static int vb10;
#pragma acc declare create (vb10)

#pragma acc routine seq
int
func4 (int a)
{
  vb8 = a + 1;
  vb9 = vb8 + 1;
  vb10 = vb9 + 1;

  return vb10;
}

int vb11;
#pragma acc declare device_resident (vb11)
extern int vb12;
#pragma acc declare device_resident (vb12)
extern int vb13;
#pragma acc declare device_resident (vb13)

#pragma acc routine seq
int
func5 (int a)
{
  vb11 = a + 1;
  vb12 = vb11 + 1;
  vb13 = vb12 + 1;

  return vb13;
}

#pragma acc routine seq
int
func6 (int a)
{
  extern int vb14;
#pragma acc declare create (vb14)
  static int vb15;
#pragma acc declare create (vb15)

  vb14 = a + 1;
  vb15 = vb14 + 1;

  return vb15;
}

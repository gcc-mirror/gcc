/* Test for various situations where a new declaration of an
   identifier conflicts with an earlier declaration which isn't in the
   same scope.  These are all undefined behavior per C89 sections
   6.1.2.2p7, 6.1.2.6p2, and 6.3.2.2p2/footnote 38 (C99 6.2.2p7 and
   6.2.7p2 - implicit declarations are invalid in C99).  */

/* { dg-do compile } */
/* { dg-options "-std=c89 -pedantic -Wall -Wno-unused" } */

/* Extern at function scope, clashing with extern at file scope */

extern int foo1;		/* { dg-error "previous" } */
extern int bar1(int);		/* { dg-error "previous" } */

void test1(void)
{
  extern double foo1;		/* { dg-error "conflict" } */
  extern double bar1(double);	/* { dg-error "conflict" } */
}

/* Extern at file scope, clashing with extern at function scope */

void test2(void)
{
  extern double foo2;		/* { dg-error "previous" } */
  extern double bar2(double);	/* { dg-error "previous" } */
}

extern int foo2;		/* { dg-error "conflict" } */
extern int bar2(int);		/* { dg-error "conflict" } */

/* Extern at function scope, clashing with extern at earlier function
   scope.  Also, don't be fooled by a typedef at file scope.  */

typedef float baz3;		/* { dg-bogus } */

void prime3(void)
{
  extern int foo3;		/* { dg-error "previous" } */
  extern int bar3(int);		/* { dg-error "previous" } */
  extern int baz3;		/* { dg-error "previous" } */
}

void test3(void)
{
  extern double foo3;		/* { dg-error "conflict" } */
  extern double bar3(double);	/* { dg-error "conflict" } */
  extern double baz3;		/* { dg-error "conflict" } */
}

/* Extern at function scope, clashing with previous implicit decl.  */

void prime4(void)
{
  bar4();			/* { dg-error "previous|implicit" } */
}

void test4(void)
{
  extern double bar4(double);	/* { dg-error "conflict" } */
}

/* Implicit decl, clashing with extern at previous function scope.  */

void prime5(void)
{
  extern double bar5(double);	/* { dg-error "previous" "" { xfail *-*-* } } */
}

void test5(void)
{
  bar5(1);			/* { dg-error "implicit" } */
}

/* Extern then static, both at file scope.  */

extern int test6(int);		/* { dg-warning "previous" "" } */
static int test6(int x)			
{ return x; }			/* { dg-warning "follows non-static" } */


/* Extern then static, extern at previous function scope.  */

void prime7(void)
{
  extern int test7(int);	/* { dg-warning "previous" "" } */
}

static int test7(int x)
{ return x; }			/* { dg-warning "follows non-static" } */

/* Implicit decl then static.  */

void prime8(void)
{
  test8();			/* { dg-warning "previous" "" } */
                                /* { dg-warning "implicit" "" { target *-*-* } 96 } */
}

static int test8(int x)
{ return x; }			/* { dg-warning "follows non-static" } */

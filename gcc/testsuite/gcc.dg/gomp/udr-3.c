/* { dg-do compile } */
/* { dg-options "-fopenmp -std=gnu89" } */

struct S { int s; };
struct T { int t; };
struct U { int u; };

#pragma omp declare reduction (+: struct S: omp_out.s += omp_in.s)
#pragma omp declare reduction (*: struct S: omp_out.s *= omp_in.s) \
		    initializer (omp_priv = {1})
#pragma omp declare reduction (foo: struct S: omp_out.s += omp_in.s)

void
f1 ()
{
  struct S s, s2;
  struct T t;
  #pragma omp declare reduction (+: struct T: omp_out.t += omp_in.t)
  #pragma omp parallel reduction (+: t) reduction (foo: s) reduction (*: s2)
  s.s = 1, t.t = 1, s2.s = 2;
  #pragma omp parallel reduction (+: s)
  s.s = 1;
}

void bar (struct S *);

void
f2 ()
{
  #pragma omp declare reduction (foo: struct S: omp_out.s += omp_in.s) initializer (bar (&omp_priv))
  #pragma omp declare reduction (bar: struct S: omp_out.s += omp_in.s) initializer (bar (&omp_orig)) /* { dg-error "one of the initializer call arguments should be" } */
}

#pragma omp declare reduction (+: struct U: omp_out.u *= omp_in.u)		/* { dg-error "previous" } */
#pragma omp declare reduction (+: struct U: omp_out.u += omp_in.u)		/* { dg-error "redeclaration of" } */

void
f3 ()
{
  #pragma omp declare reduction (f3: struct U: omp_out.u *= omp_in.u)		/* { dg-error "previous" } */
  #pragma omp declare reduction (f3: struct U: omp_out.u += omp_in.u)		/* { dg-error "redeclaration of" } */
}

struct V
{
  #pragma omp declare reduction (bar: struct S: omp_out.s *= omp_in.s)		/* { dg-error "not at file or block scope" } */
  #pragma omp declare reduction (bar: struct S: omp_out.s += omp_in.s)		/* { dg-error "not at file or block scope" } */
};

#pragma omp declare reduction (n3: long: omp_out += omp_in)		/* { dg-error "previous" } */
#pragma omp declare reduction (n3: long int: omp_out += omp_in)		/* { dg-error "redeclaration of" } */
#pragma omp declare reduction (n3: short unsigned: omp_out += omp_in)
#pragma omp declare reduction (n3: short int: omp_out += omp_in)

void
f4 (void)
{
  #pragma omp declare reduction (f4: long: omp_out += omp_in)		/* { dg-error "previous" } */
  #pragma omp declare reduction (f4: long int: omp_out += omp_in)	/* { dg-error "redeclaration of" } */
  #pragma omp declare reduction (f4: short unsigned: omp_out += omp_in)
  #pragma omp declare reduction (f4: short int: omp_out += omp_in)
}

void
f5 (void)
{
  #pragma omp declare reduction (+: struct S: omp_out.s += omp_in.s) initializer (omp_priv) /* { dg-error "expected" } */
  #pragma omp declare reduction (+: struct T: omp_out.t += omp_in.t) initializer (omp_priv ()) /* { dg-error "expected" } */
}

void
f6 (a, b)
#pragma omp declare reduction (bar: struct S: omp_out.s *= omp_in.s)	/* { dg-error "expected declaration specifiers before" } */
  int a;
  int b;
{
}

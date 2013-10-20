/* { dg-do compile } */
/* { dg-options "-fopenmp" } */

#pragma omp declare reduction (| : long int : omp_out |= omp_in)	/* { dg-error "predeclared arithmetic type" } */
#pragma omp declare reduction (+ : char : omp_out += omp_in)		/* { dg-error "predeclared arithmetic type" } */
typedef short T;
#pragma omp declare reduction (min : T : omp_out += omp_in)		/* { dg-error "predeclared arithmetic type" } */
#pragma omp declare reduction (* : _Complex double : omp_out *= omp_in)	/* { dg-error "predeclared arithmetic type" } */

void
foo (void)
{
  #pragma omp declare reduction (| : long int : omp_out |= omp_in)	/* { dg-error "predeclared arithmetic type" } */
  #pragma omp declare reduction (+ : char : omp_out += omp_in)		/* { dg-error "predeclared arithmetic type" } */
  #pragma omp declare reduction (min : T : omp_out += omp_in)		/* { dg-error "predeclared arithmetic type" } */
  #pragma omp declare reduction (* : _Complex double : omp_out *= omp_in) /* { dg-error "predeclared arithmetic type" } */
}

#pragma omp declare reduction (| : __typeof (foo) : omp_out |= omp_in)	/* { dg-error "function or array" } */
#pragma omp declare reduction (+ : char () : omp_out += omp_in)		/* { dg-error "function or array" } */
#pragma omp declare reduction (min : T[2] : omp_out += omp_in)		/* { dg-error "function or array" } */

void
bar (void)
{
  #pragma omp declare reduction (| : __typeof (foo) : omp_out |= omp_in)/* { dg-error "function or array" } */
  #pragma omp declare reduction (+ : char () : omp_out += omp_in)	/* { dg-error "function or array" } */
  #pragma omp declare reduction (min : T[2] : omp_out += omp_in)	/* { dg-error "function or array" } */
}

struct A { int a; };
#pragma omp declare reduction (| : const struct A : omp_out.a |= omp_in.a)	/* { dg-error "const, volatile or restrict" } */
#pragma omp declare reduction (+ : __const struct A : omp_out.a += omp_in.a)	/* { dg-error "const, volatile or restrict" } */
typedef volatile struct A T2;
#pragma omp declare reduction (min : T2 : omp_out.a += omp_in.a)		/* { dg-error "const, volatile or restrict" } */
#pragma omp declare reduction (* : struct A *__restrict : omp_out->a *= omp_in->a)/* { dg-error "const, volatile or restrict" } */

void
baz (void)
{
  #pragma omp declare reduction (| : const struct A : omp_out.a |= omp_in.a)	/* { dg-error "const, volatile or restrict" } */
  #pragma omp declare reduction (+ : __const struct A : omp_out.a += omp_in.a)	/* { dg-error "const, volatile or restrict" } */
  typedef volatile struct A T3;
  #pragma omp declare reduction (min : T3 : omp_out.a += omp_in.a)		/* { dg-error "const, volatile or restrict" } */
  #pragma omp declare reduction (* : struct A *__restrict : omp_out->a *= omp_in->a)/* { dg-error "const, volatile or restrict" } */
}

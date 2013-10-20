/* { dg-do compile } */
/* { dg-options "-fopenmp" } */

struct W { int w; };
void init (struct W *, int, int *);
int v;
#pragma omp declare reduction (foo : long int : omp_out |= v)	/* { dg-error "combiner refers to variable" } */
#pragma omp declare reduction (foo : char : omp_out = v)	/* { dg-error "combiner refers to variable" } */
typedef short T;
#pragma omp declare reduction (foo : T : omp_out += v)	/* { dg-error "combiner refers to variable" } */
#pragma omp declare reduction (foo : int : v *= omp_in)	/* { dg-error "combiner refers to variable" } */
#pragma omp declare reduction (foo : struct W : omp_out.w *= omp_in.w + v) /* { dg-error "combiner refers to variable" } */

void
foo (int v)
{
  #pragma omp declare reduction (foo : long int : omp_out |= v)	/* { dg-error "combiner refers to variable" } */
  #pragma omp declare reduction (foo : char : omp_out = v)	/* { dg-error "combiner refers to variable" } */
  #pragma omp declare reduction (foo : T : omp_out += v)	/* { dg-error "combiner refers to variable" } */
  #pragma omp declare reduction (foo : int : v *= omp_in)	/* { dg-error "combiner refers to variable" } */
  #pragma omp declare reduction (foo : struct W : omp_out.w *= omp_in.w + v) /* { dg-error "combiner refers to variable" } */
}

#pragma omp declare reduction (bar : long int : omp_out |= omp_in) initializer (omp_priv = v) /* { dg-error "initializer refers to variable" } */
#pragma omp declare reduction (bar : char : omp_out += omp_in) initializer (omp_priv = ((char) v)) /* { dg-error "initializer refers to variable" } */
#pragma omp declare reduction (bar : T : omp_out += omp_in) initializer (omp_priv = (short) v) /* { dg-error "initializer refers to variable" } */
#pragma omp declare reduction (bar : _Complex double : omp_out *= omp_in) initializer (omp_priv = (v)) /* { dg-error "initializer refers to variable" } */
#pragma omp declare reduction (bar : struct W : omp_out.w *= omp_in.w) initializer (omp_priv = { v } ) /* { dg-error "initializer refers to variable" } */
#pragma omp declare reduction (bar2 : struct W : omp_out.w *= omp_in.w) initializer (init (&omp_priv, v, (int *) 0)) /* { dg-error "initializer refers to variable" } */
#pragma omp declare reduction (bar3 : struct W : omp_out.w *= omp_in.w) initializer (init (&omp_priv, 0, &v)) /* { dg-error "initializer refers to variable" } */

void
bar (int v)
{
  #pragma omp declare reduction (bar : long int : omp_out |= omp_in) initializer (omp_priv = v) /* { dg-error "initializer refers to variable" } */
  #pragma omp declare reduction (bar : char : omp_out += omp_in) initializer (omp_priv = ((char) v)) /* { dg-error "initializer refers to variable" } */
  #pragma omp declare reduction (bar : T : omp_out += omp_in) initializer (omp_priv = (short) v) /* { dg-error "initializer refers to variable" } */
  #pragma omp declare reduction (bar : _Complex double : omp_out *= omp_in) initializer (omp_priv = (v)) /* { dg-error "initializer refers to variable" } */
  #pragma omp declare reduction (bar : struct W : omp_out.w *= omp_in.w) initializer (omp_priv = { v }) /* { dg-error "initializer refers to variable" } */
  #pragma omp declare reduction (bar2 : struct W : omp_out.w *= omp_in.w) initializer (init (&omp_priv, v, (int *) 0)) /* { dg-error "initializer refers to variable" } */
  #pragma omp declare reduction (bar3 : struct W : omp_out.w *= omp_in.w) initializer (init (&omp_priv, 0, &v)) /* { dg-error "initializer refers to variable" } */
}

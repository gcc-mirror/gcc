/* omp_interop_t undefined (on purpose).  */

float repl0(short, short);
#pragma omp declare variant(repl0) match(construct={dispatch}) append_args(interop(target), interop(targetsync))
float base0();
/* { dg-error "argument 1 of 'repl0' must be of 'omp_interop_t'" "" { target c } .-3 }  */
/* { dg-error "argument 1 of 'repl0' must be of 'omp_interop_t'" "" { target c++ } .-4 }  */
/* { dg-note "'append_args' specified here" "" { target *-*-* } .-4 } */

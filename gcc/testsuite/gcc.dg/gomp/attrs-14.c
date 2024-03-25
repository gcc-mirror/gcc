/* PR c++/102413 */
/* { dg-do compile } */
/* { dg-options "-fopenmp -std=c23" } */

[[omp::directive(error]];	/* { dg-error "expected|declare" } */

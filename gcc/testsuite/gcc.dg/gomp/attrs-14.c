/* PR c++/102413 */
/* { dg-do compile } */
/* { dg-options "-fopenmp -std=c2x" } */

[[omp::directive(error]];	/* { dg-error "expected|declare" } */

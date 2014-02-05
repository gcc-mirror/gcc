/* PR c++/58703 */
/* { dg-do compile } */
/* { dg-options "-fopenmp" } */

#pragma omp declare reduction (+ : char[] : omp_out += omp_in) /* { dg-error "function or array type" } */
#pragma omp declare reduction (+ : char() : omp_out += omp_in) /* { dg-error "function or array type" } */

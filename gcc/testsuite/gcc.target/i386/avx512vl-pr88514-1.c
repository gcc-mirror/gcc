/* PR target/88514 */
/* { dg-do assemble { target avx512vl } } */
/* { dg-options "-Ofast -mavx512vl -mtune=intel -mprefer-vector-width=128" } */

#include "avx512vl-pr79299-1.c"

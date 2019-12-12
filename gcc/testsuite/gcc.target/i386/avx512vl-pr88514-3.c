/* PR target/88514 */
/* { dg-do assemble { target avx512vl } } */
/* { dg-options "-Ofast -mavx512vl -mtune=intel -mprefer-vector-width=512" } */

#include "avx512vl-pr79299-1.c"

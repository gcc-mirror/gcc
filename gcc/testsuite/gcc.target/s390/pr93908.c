/* PR rtl-optimization/93908 */
/* { dg-do run { target s390_zEC12_hw } } */
/* { dg-options "-O2 -march=zEC12 -mtune=z13" } */

#include "../../gcc.c-torture/execute/pr93908.c"

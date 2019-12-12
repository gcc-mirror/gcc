/* PR tree-optimization/89268 */
/* { dg-do compile } */
/* { dg-require-effective-target vect_condition } */
/* { dg-additional-options "-fno-trapping-math --param vect-epilogues-nomask=1" } */
/* { dg-additional-options "-mavx512ifma -mtune=intel" { target x86_64-*-* i?86-*-* } } */

#include "pr79887.c"

/* { dg-do run } */
/* { dg-add-options vect_early_break } */
/* { dg-require-effective-target vect_early_break_hw } */
/* { dg-additional-options "-O3 -fvect-cost-model=unlimited" } */

#include "vect-early-break-pr128028.c"

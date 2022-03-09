/* PR c/65179 */
/* { dg-do compile } */
/* { dg-options "-O -Wshift-negative-value -fwrapv" } */
/* { dg-additional-options "-std=c++03" { target c++ } } */
/* { dg-additional-options "-std=c90" { target c } } */

#include "Wshift-negative-value-1.c"

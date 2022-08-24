/* PR c++/55095 */
/* { dg-do compile { target int32 } } */
/* { dg-options "-O -Wshift-overflow -fwrapv" } */
/* { dg-additional-options "-std=gnu90" { target c } } */
/* { dg-additional-options "-std=c++03" { target c++ } } */

#include "Wshift-overflow-1.c"

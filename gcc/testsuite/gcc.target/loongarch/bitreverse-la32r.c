/* Verify that bitreverse builtins do not ICE on LA32R.  */
/* { dg-do compile } */
/* { dg-options "-mabi=ilp32s -march=la32rv1.0" } */
#include "la64/bitreverse.c"

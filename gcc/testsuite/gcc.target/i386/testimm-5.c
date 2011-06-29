/* PR target/49411 */
/* { dg-do assemble } */
/* { dg-options "-O2 -mf16c -maes -mpclmul" } */
/* { dg-require-effective-target f16c } */
/* { dg-require-effective-target vaes } */
/* { dg-require-effective-target vpclmul } */

#include "testimm-4.c"

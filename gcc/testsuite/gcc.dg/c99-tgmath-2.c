/* Test for <tgmath.h> in C99. */
/* Origin: Matt Austern <austern@apple.com>
/* { dg-do compile { target c99_runtime } } */
/* { dg-options "-std=iso9899:1999" } */
/* { dg-add-options c99_runtime } */
/* { dg-skip-if "<complex.h> missing" { alpha*-dec-osf5* } } */

/* Test that invoking type-generic sin on a float invokes sinf. */
#include <tgmath.h>

float foo(float x)
{
  return sin(x);
}

/* { dg-final { scan-assembler "sinf" } } */

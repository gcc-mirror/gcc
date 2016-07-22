/* { dg-do run { target { powerpc*-*-linux* } } } */
/* { dg-require-effective-target vsx_hw } */
/* { dg-options "-O2 -mvsx" } */

#define TYPE double
#define ELEMENTS 2
#define INITIAL { 10.0, -20.0 }

#include "vec-extract.h"

/* { dg-do run { target { powerpc*-*-linux* } } } */
/* { dg-require-effective-target vsx_hw } */
/* { dg-options "-O2 -mvsx" } */

#define TYPE int
#define ELEMENTS 4
#define INITIAL { 10, -20, 30, -40 }

#include "vec-extract.h"

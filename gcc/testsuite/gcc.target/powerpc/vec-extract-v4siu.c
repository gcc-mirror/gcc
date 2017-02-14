/* { dg-do run { target { powerpc*-*-linux* } } } */
/* { dg-require-effective-target vsx_hw } */
/* { dg-options "-O2 -mvsx" } */

#define TYPE unsigned int
#define ELEMENTS 4
#define INITIAL { 1, 2, 0xff03, 0xff04 }

#include "vec-extract.h"

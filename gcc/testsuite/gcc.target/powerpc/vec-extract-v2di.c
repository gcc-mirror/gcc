/* { dg-do run { target lp64 } } */
/* { dg-require-effective-target vsx_hw } */
/* { dg-options "-O2 -mvsx" } */

#define TYPE long
#define ELEMENTS 2
#define INITIAL { 10, -20 }

#include "vec-extract.h"

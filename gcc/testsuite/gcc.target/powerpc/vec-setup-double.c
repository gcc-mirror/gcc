/* { dg-do run { target { powerpc*-*-linux* } } } */
/* { dg-require-effective-target vsx_hw } */
/* { dg-options "-O2 -mvsx" } */

/* Test various ways of creating vectors with 2 double words and accessing the
   elements.  This test uses the double datatype and the default endian
   order.  */

#define DO_DOUBLE

#include "vec-setup.h"

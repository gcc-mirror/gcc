/* { dg-do run } */
/* { dg-require-effective-target vsx_hw } */
/* { dg-options "-O2 -mvsx" } */

/* Test various ways of creating vectors with 2 double words and accessing the
   elements.  This test uses the long (on 64-bit systems) or long long datatype
   (on 32-bit systems).  The default endian order is used.  */

#include "vec-setup.h"

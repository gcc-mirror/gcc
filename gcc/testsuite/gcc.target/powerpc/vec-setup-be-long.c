/* { dg-do run { target { powerpc64le*-*-linux* } } } */
/* { dg-require-effective-target vsx_hw } */
/* { dg-options "-O2 -mvsx -maltivec=be" } */

/* Test various ways of creating vectors with 2 double words and accessing the
   elements.  This test uses the long (on 64-bit systems) or long long datatype
   (on 32-bit systems).

   This test explicitly tests -maltivec=be to make sure things are correct.  */

#include "vec-setup.h"

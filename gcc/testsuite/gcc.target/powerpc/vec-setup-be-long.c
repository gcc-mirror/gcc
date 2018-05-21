/* Per PR78303, we are deprecating usage of -maltivec=be on little endian,
   so XFAIL this test until support is actually removed.
   Update: Fix for PR85698 fixes this testcase on LE so XFAIL removed.  */
/* { dg-do run { target { powerpc64le*-*-linux* } } } */
/* { dg-require-effective-target vsx_hw } */
/* Disable warnings to squelch deprecation message about -maltivec=be.  */
/* { dg-options "-w -O2 -mvsx -maltivec=be" } */

/* Test various ways of creating vectors with 2 double words and accessing the
   elements.  This test uses the long (on 64-bit systems) or long long datatype
   (on 32-bit systems).

   This test explicitly tests -maltivec=be to make sure things are correct.  */

#include "vec-setup.h"

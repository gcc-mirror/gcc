/* { dg-do compile } */
/* { dg-skip-if "Cannot access arbitrary stack frames" { arm*-*-* hppa*-*-* ia64-*-* visium-*-* } } */
/* { dg-options "-Werror" } */
/* { dg-additional-options "-mbackchain" { target { s390*-*-* } } } */

/* Verify that -Wframe-address is not enabled by default by enabling
   -Werror and verifying the test still compiles.  */
#include "Wframe-address.c"

/* { dg-do compile } */
/* { dg-options "-Werror" } */

/* Verify that -Wframe-address is not enabled by default by enabling
   -Werror and verifying the test still compiles.  */
#include "Wframe-address.c"

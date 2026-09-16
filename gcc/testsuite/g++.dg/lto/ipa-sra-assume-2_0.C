/* Test that the assume can be copied before creating the edges.  */

/* { dg-lto-do assemble } */
/* { dg-lto-options  {{ -O3 -flto }} } */

#include "../ipa/ipa-sra-assume-2.C"

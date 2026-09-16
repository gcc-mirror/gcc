/* Test that the assume can be copied after creating the edges.  */

/* { dg-lto-do assemble } */
/* { dg-lto-options  {{ -O3 -fno-early-inlining -flto }} } */

#include "../ipa/ipa-sra-assume-3.C"

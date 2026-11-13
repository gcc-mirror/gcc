/* Regression test to ensure we don't ICE on this code (we don't care
   about the output).  */
/* { dg-additional-options "-Wno-analyzer-use-of-uninitialized-value" } */

#include "../../../c-c++-common/pr59037.c"

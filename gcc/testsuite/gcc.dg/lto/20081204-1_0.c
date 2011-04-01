/* { dg-lto-do link } */
/* { dg-require-effective-target fpic } */
/* { dg-lto-options {{-flto -flto-partition=1to1 -fPIC -r -nostdlib}} } */

/* Tests for the absence during linking of:
   lto1: error: type of 'i' does not match original declaration  */

const int i[1];

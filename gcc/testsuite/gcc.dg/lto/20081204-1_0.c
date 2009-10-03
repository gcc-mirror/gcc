/* { dg-lto-do link } */
/* { dg-lto-options {{-fwhopr -fPIC -shared}} } */

/* Tests for the absence during linking of:
   lto1: error: type of 'i' does not match original declaration  */

const int i[1];

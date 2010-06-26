/* Parameters of -Xassembler should not be interpreted as driver
   options (and so cause the driver to exit prematurely, as in this
   testcase, or have other unintended effects).  */
/* { dg-do compile } */
/* { dg-options "-Xassembler -dumpmachine" } */

int int x; /* { dg-error "two or more data types" } */

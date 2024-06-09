/* { dg-do compile } */
/* { dg-options "-fsplit-stack" } */
/* { dg-require-effective-target strub } */
/* { dg-require-effective-target split_stack } */

void __attribute__ ((__strub__))
f () {} /* { dg-message "not eligible|requested" } */

void __attribute__ ((__strub__ ("internal")))
g () {} /* { dg-message "not eligible|requested" } */

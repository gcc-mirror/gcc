/* { dg-do compile } */
/* { dg-require-ifunc "" } */
/* { dg-options "-O0" } */

float foo () { return 1; } /* { dg-message "previous definition of .foo." } */

__attribute__ ((target_clones ("default", "dotprod", "sve"))) float
foo () { return 3; } /* { dg-error "redefinition of .foo \\\[\\\[target_clones\\(.default., .dotprod., .sve.\\)\\\]\\\]." } */

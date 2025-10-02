/* { dg-do compile } */
/* { dg-require-ifunc "" } */
/* { dg-options "-O0" } */

float foo () { return 1; } /* { dg-message ".float foo\\(\\). previously defined here" } */

__attribute__ ((target_clones ("default", "dotprod", "sve"))) float
foo () { return 3; } /* { dg-error "redefinition of .float foo \\\[\\\[target_clones\\(.default., .dotprod., .sve.\\)\\\]\\\] \\(\\)." } */

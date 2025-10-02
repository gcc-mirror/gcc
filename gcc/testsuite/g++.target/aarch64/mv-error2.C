/* { dg-do compile } */
/* { dg-require-ifunc "" } */
/* { dg-options "-O0" } */

__attribute__ ((target_version ("dotprod"))) float
foo () { return 3; } /* { dg-message ".float foo \\\[\\\[target_version\\(.dotprod.\\)\\\]\\\] \\(\\). previously defined here" } */

__attribute__ ((target_version ("dotprod"))) float
foo () { return 3; } /* { dg-error "redefinition of .float foo \\\[\\\[target_version\\(.dotprod.\\)\\\]\\\] \\(\\)." } */

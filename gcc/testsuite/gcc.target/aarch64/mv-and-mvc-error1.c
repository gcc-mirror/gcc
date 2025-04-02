/* { dg-do compile } */
/* { dg-require-ifunc "" } */
/* { dg-options "-O0" } */

__attribute__ ((target_version ("dotprod"))) int
foo () { return 3; } /* { dg-message "previous definition of .foo \\\[\\\[target_version\\(.dotprod.\\)\\\]\\\]. with type .int\\(void\\)." } */

__attribute__ ((target_clones ("dotprod", "sve"))) int
foo () { return 1; } /* { dg-error "redefinition of .foo \\\[\\\[target_clones\\(.dotprod., .sve.\\)\\\]\\\]." } */

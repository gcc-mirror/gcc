/* { dg-do compile } */
/* { dg-require-ifunc "" } */
/* { dg-options "-O0" } */

__attribute__ ((target_version ("dotprod"))) float
foo () { return 3; } /* { dg-message "previous definition of .foo \\\[\\\[target_version\\(.dotprod.\\)\\\]\\\]. with type .float\\(void\\)." } */

__attribute__ ((target_version ("dotprod"))) float
foo () { return 3; } /* { dg-error "redefinition of .foo \\\[\\\[target_version\\(.dotprod.\\)\\\]\\\]." } */

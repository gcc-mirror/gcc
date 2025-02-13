/* { dg-do compile } */
/* { dg-require-ifunc "" } */
/* { dg-options "-O0" } */

__attribute__ ((target_version ("dotprod"))) int
foo () { return 3; } /* { dg-message "previous declaration of .int foo \\\[\\\[target_version\\(.dotprod.\\)\\\]\\\] \\(\\)." } */

__attribute__ ((target_clones ("dotprod", "sve"))) int
foo () { return 1; } /* { dg-error ".int foo \\\[\\\[target_clones\\(.dotprod., .sve.\\)\\\]\\\] \\(\\). conflicts for version .dotprod." } */

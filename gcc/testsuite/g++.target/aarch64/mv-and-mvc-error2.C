/* { dg-do compile } */
/* { dg-require-ifunc "" } */
/* { dg-options "-O0" } */

__attribute__ ((target_version ("default"))) int
foo () { return 1; } /* { dg-message "old declaration .int foo \\\[\\\[target_version\\(.default.\\)\\\]\\\] \\(\\)." } */

__attribute__ ((target_clones ("dotprod", "sve"))) float
foo () { return 3; } /* { dg-error "ambiguating new declaration of .float foo \\\[\\\[target_clones\\(.dotprod., .sve.\\)\\\]\\\] \\(\\)." } */

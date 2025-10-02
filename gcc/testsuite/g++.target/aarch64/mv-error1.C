/* { dg-do compile } */
/* { dg-require-ifunc "" } */
/* { dg-options "-O0" } */

__attribute__ ((target_version ("default"))) int
foo ();

__attribute__ ((target_version ("default"))) int
foo () { return 1; } /* { dg-message "old declaration .int foo \\\[\\\[target_version\\(.default.\\)\\\]\\\] \\(\\)." } */

__attribute__ ((target_version ("dotprod"))) float
foo () { return 3; } /* { dg-error "ambiguating new declaration of .float foo \\\[\\\[target_version\\(.dotprod.\\)\\\]\\\] \\(\\)." } */

__attribute__ ((target_version ("sve"))) int
foo2 () { return 1; } /* { dg-message "old declaration" } */

__attribute__ ((target_version ("dotprod"))) float
foo2 () { return 3; } /* { dg-error "ambiguating new declaration of" } */

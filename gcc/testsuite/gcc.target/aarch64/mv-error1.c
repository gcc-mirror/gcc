/* { dg-do compile } */
/* { dg-require-ifunc "" } */
/* { dg-options "-O0" } */

__attribute__ ((target_version ("default"))) int
foo ();

__attribute__ ((target_version ("default"))) int
foo () { return 1; } /* { dg-message "previous definition of .foo \\\[\\\[target_version\\(.default.\\)\\\]\\\]. with type .int\\(void\\)." } */

__attribute__ ((target_version ("dotprod"))) float
foo () { return 3; } /* { dg-error "conflicting types for .foo \\\[\\\[target_version\\(.dotprod.\\)\\\]\\\].; have .float\\(void\\)." } */

__attribute__ ((target_version ("sve"))) int
foo2 () { return 1; } /* { dg-message "previous definition of .foo2 \\\[\\\[target_version\\(.sve.\\)\\\]\\\]. with type .int\\(void\\)." } */

__attribute__ ((target_version ("dotprod"))) float
foo2 () { return 3; } /* { dg-error "conflicting types for .foo2 \\\[\\\[target_version\\(.dotprod.\\)\\\]\\\].; have .float\\(void\\)." } */

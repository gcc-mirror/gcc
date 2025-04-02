/* { dg-do compile } */
/* { dg-require-ifunc "" } */
/* { dg-options "-O0" } */

__attribute__ ((target_version ("default"))) int
foo () { return 1; } /* { dg-message "previous definition of .foo \\\[\\\[target_version\\(.default.\\)\\\]\\\]. with type .int\\(void\\)." } */

__attribute__ ((target_clones ("dotprod", "sve"))) float
foo () { return 3; } /* { dg-error "conflicting types for .foo \\\[\\\[target_clones\\(.dotprod., .sve.\\)\\\]\\\].; have .float\\(void\\)." } */

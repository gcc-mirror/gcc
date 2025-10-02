/* { dg-do compile } */
/* { dg-require-ifunc "" } */
/* { dg-options "-O0" } */

__attribute__ ((target_clones ("default, dotprod"))) float
foo (); /* { dg-message "previous declaration of .float foo \\\[\\\[target_clones\\(.default., .dotprod.\\)\\\]\\\] \\(\\)." } */

__attribute__ ((target_clones ("dotprod", "sve"))) float
foo () { return 3; } /* { dg-error ".float foo \\\[\\\[target_clones\\(.dotprod., .sve.\\)\\\]\\\] \\(\\). conflicts with overlapping .target_clone. declaration" } */

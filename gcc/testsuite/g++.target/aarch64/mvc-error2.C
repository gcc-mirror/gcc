/* { dg-do compile } */
/* { dg-require-ifunc "" } */
/* { dg-options "-O0" } */

__attribute__ ((target_clones ("default, dotprod"))) float
foo () { return 3; } /* { dg-message "previous declaration" } */

__attribute__ ((target_clones ("dotprod", "sve"))) float
foo () { return 3; } /* { dg-error "conflicts with overlapping .target_clone. declaration" } */

/* { dg-do compile } */
/* { dg-require-ifunc "" } */
/* { dg-options "-O0" } */

__attribute__ ((target_version ("dotprod"))) float
foo () { return 3; }

__attribute__ ((target_version ("default"))) float
foo () { return 3; } /* { dg-message "previously defined here" } */

__attribute__ ((target_version ("default"))) float
foo () { return 3; } /* { dg-error "redefinition of" } */

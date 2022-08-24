/* { dg-do preprocess } */
/* { dg-additional-options "-fdiagnostics-show-caret" } */

const char *s = "";

/* { dg-warning "missing terminating \"" "test1" { target *-*-* } 4 } */
/* { dg-warning "missing terminating \"" "test2" { target *-*-* } 5 } */

/* { dg-begin-multiline-output "test3" }
 const char *s = "
                 ^
{ dg-end-multiline-output "test3" } */

/* { dg-begin-multiline-output "test4" }
 ";
 ^
{ dg-end-multiline-output "test4" } */

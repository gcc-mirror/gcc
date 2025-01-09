/* { dg-do compile } */
/* { dg-require-ifunc "" } */
/* { dg-options "-O0" } */

__attribute__((target_clones("default", "sve")))
int foo () { return 1; }/* { dg-warning "Function Multi Versioning support is experimental, and the behavior is likely to change" } */

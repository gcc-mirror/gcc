/* { dg-do compile } */
/* { dg-require-ifunc "" } */
/* { dg-options "-O0" } */

__attribute__((target_clones("default", "dotprod", "sve+sve2")))
int foo () {
  return 1;
}

__attribute__((target_clones("invalid1")))
int foo () { /* { dg-warning "invalid feature modifier .invalid1. in version .invalid1. for .target_clones. attribute" } */
  return 2;
}

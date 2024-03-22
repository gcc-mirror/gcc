/* { dg-do compile } */
/* { dg-require-ifunc "" } */

__attribute__((target_clones("avx","arch=slm","default")))
__attribute__((target("avx")))
int foo (); /* { dg-warning "ignoring attribute 'target' because it conflicts with attribute 'target_clones'" } */

__attribute__((target_clones("avx","arch=slm","default"),always_inline))
int bar (); /* { dg-warning "ignoring attribute 'always_inline' because it conflicts with attribute 'target_clones'" } */

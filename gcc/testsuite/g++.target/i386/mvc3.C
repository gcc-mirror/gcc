/* { dg-do compile } */
/* { dg-require-ifunc "" } */

__attribute__((target("avx")))
__attribute__((target_clones("avx","arch=slm","default")))
int foo (); /* { dg-warning "ignoring attribute 'target_clones' because it conflicts with attribute 'target'" } */

__attribute__((always_inline,target_clones("avx","arch=slm","default")))
int bar (); /* { dg-warning "ignoring attribute 'target_clones' because it conflicts with attribute 'always_inline'" } */

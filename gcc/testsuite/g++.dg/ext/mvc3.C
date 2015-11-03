/* { dg-do compile { target i?86-*-* x86_64-*-* } } */

__attribute__((target("avx")))
__attribute__((target_clones("avx","arch=slm","default")))
int foo (); /* { dg-warning "'target_clones' attribute ignored due to conflict with 'target' attribute" } */

__attribute__((always_inline,target_clones("avx","arch=slm","default")))
int bar (); /* { dg-warning "'target_clones' attribute ignored due to conflict with 'always_inline' attribute" } */

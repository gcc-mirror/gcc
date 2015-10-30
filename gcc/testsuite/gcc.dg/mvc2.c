/* { dg-do compile { target i?86-*-* x86_64-*-* } } */

__attribute__((target_clones("avx","arch=slm","arch=core-avx2")))
int foo ();

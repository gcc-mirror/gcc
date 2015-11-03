/* { dg-do compile } */

__attribute__((target_clones("avx","arch=slm","arch=core-avx2")))
int foo ();

/* { dg-do compile } */
/* { dg-options "-O2 -march=x86-64-v3" } */

#pragma GCC push_options
#pragma GCC target("avx10.2")

void foo();

#pragma GCC pop_options

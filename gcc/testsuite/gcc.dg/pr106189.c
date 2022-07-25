/* { dg-do compile } */
/* { dg-options "-O2 -Warray-bounds=2 -w" } */

int a_n_0_0_a[][0];
void a_n_0_0() { T(((char *)a_n_0_0_a)[1]); }

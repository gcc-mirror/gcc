/* { dg-do compile } */
/* { dg-options "-O2 -mno-sse" } */

#pragma GCC target("sse4.1")
#include <immintrin.h>
int main(){return 0;}

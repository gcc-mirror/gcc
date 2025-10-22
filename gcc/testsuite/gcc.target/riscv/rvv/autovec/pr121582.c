/* { dg-do compile } */
/* { dg-options "-O3 -march=rv64gcv_zvfbfmin -mabi=lp64d" } */

typedef __attribute__((__vector_size__(8))) __bf16 V;
V v, w;
void foo() { v -= w; }

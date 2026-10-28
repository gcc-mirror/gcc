/* { dg-do compile } */
/* { dg-options "-march=rv64gc_zbkb -mabi=lp64" } */
unsigned long f(unsigned long x){ return x & 0xFF000000000000FFul; }

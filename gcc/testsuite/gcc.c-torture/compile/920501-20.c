/* { dg-additional-options "-std=gnu89" } */

int*f(x)int*x;{if(x[4]){int h[1];if(setjmp(h))return x;}}

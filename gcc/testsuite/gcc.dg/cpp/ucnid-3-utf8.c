/* { dg-do compile } */
/* { dg-options "-std=c99" } */

#define paste(x, y) x ## y

int paste(ª, Ա) = 3;


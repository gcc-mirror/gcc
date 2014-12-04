/* { dg-do compile } */
/* { dg-options "-std=c99" } */

#define paste(x, y) x ## y

int paste(\u00aa, \u0531) = 3;


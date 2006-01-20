/* { dg-do compile } */
#include <altivec.h>
extern vector unsigned int gn00111;
long f() { return (long)&gn00111; }


/* { dg-do compile } */
#include <altivec.h>
extern vector unsigned int gn00111;
int f() { return (int)&gn00111; }


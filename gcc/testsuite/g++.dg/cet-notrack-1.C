/* { dg-do compile { target i?86-*-* x86_64-*-* } } */
/* { dg-options "-fcf-protection -mcet" } */
/* { dg-final { scan-assembler "endbr32|endbr64" } } */
/* { dg-final { scan-assembler-times "\tcall\[ \t]+puts" 2 } } */
/* { dg-final { scan-assembler-times "notrack call\[ \t]+" 1 } } */
#include <stdio.h>

struct A {
virtual int foo() __attribute__((nocf_check)) { return 42; }
};

struct B : A {
int foo() __attribute__((nocf_check)) { return 73; }
};

int main() {
B b;
A& a = b;
int (A::*amem) () __attribute__((nocf_check)) = &A::foo; // take address
if ((a.*amem)() == 73) // use the address
  printf("pass\n");
else
  printf("fail\n");
return 0;
}

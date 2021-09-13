/* { dg-do link { target aarch64_asm_sve_ok } } */
/* { dg-additional-options "-flto" } */
#include <arm_sve.h>
bool a;
int main() { a = svaddv(svptrue_b8(), svdup_s8(0)); }

/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32d -O3" } */

#include "riscv_vector.h"
void __attribute__((noinline)) foo(int arr[4]) {
__builtin_printf("%d %d %d %d\n", arr[0], arr[1], arr[2], arr[3]);
}

void __attribute__((noinline)) test() {
// Intialization with 2 memsets leads to spilling of zero-splat value
vint32m1_t a;
int arr1[4] = {};
foo(arr1);
int arr2[4] = {};
foo(arr2);
asm volatile ("# %0" : "+vr" (a));
throw int();
}

int main() {
try {
   test();
} catch (...) {
   __builtin_printf("hello\n");
};
return 0;
}

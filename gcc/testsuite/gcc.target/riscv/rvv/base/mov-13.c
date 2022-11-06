/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32d -O3" } */

#include "riscv_vector.h" 

void mov1 (int8_t *in, int8_t *out) 
{ 
 register vint8mf8_t v1 asm("v1") = *(vint8mf8_t*)in; 
 asm volatile ("# %0"::"vr"(v1)); 
 register vint8mf8_t v2 asm("v2") = v1;
 asm volatile ("#":::"v2"); 
 *(vint8mf8_t*)out = v2; 
 asm volatile ("# %0"::"vr"(v2)); 
}

/* { dg-do compile } */
/* { dg-options "-O0" } */

#include <stdint.h>
uint64_t f(uint64_t a) {
  a = a & 0xfffffffffffff;
  return a;
} 

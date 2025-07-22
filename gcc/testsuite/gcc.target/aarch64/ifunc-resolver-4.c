/* { dg-do run }  */
/* { dg-require-ifunc "" } */
/* { dg-require-effective-target mmap } */
/* { dg-options "-Wno-experimental-fmv-target" } */

#include <stdint.h>

typedef struct {
  uint64_t size;
  uint64_t hwcap;
  uint64_t hwcap2;
  uint64_t hwcap3;
  uint64_t hwcap4;
} ifunc_arg_t;

#include "ifunc-resolver.in"

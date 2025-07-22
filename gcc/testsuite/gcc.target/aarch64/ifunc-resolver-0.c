/* { dg-do run }  */
/* { dg-require-ifunc "" } */
/* { dg-require-effective-target mmap } */
/* { dg-options "-Wno-experimental-fmv-target" } */

#include <stdint.h>

typedef struct {
  uint64_t size;
} ifunc_arg_t;

#include "ifunc-resolver.in"

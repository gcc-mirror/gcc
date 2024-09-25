/* The bool bitfield type.  */

/* { dg-do compile } */
/* { dg-options "-O0 -gctf -dA" } */

/* { dg-final { scan-assembler-times "\[\t \]0x2\[\t \]+\[^\n\]*cts_type" 2 } } */
/* { dg-final { scan-assembler-times "\[\t \]0x1\[\t \]+\[^\n\]*cts_bits" 2 } } */

/* { dg-final { scan-assembler-times "ascii \"_Bool.0\"\[\t \]+\[^\n\]*ctf_string" 1 } } */

#include <stdbool.h>

struct open_file {
  bool mmapped:1;
  bool released:1;
} of;

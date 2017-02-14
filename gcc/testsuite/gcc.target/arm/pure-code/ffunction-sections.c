/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "-fpic" "-fPIC" } { "" } } */
/* { dg-options "-ffunction-sections -mpure-code" } */
#include <limits.h>

char * foo (void)
{
  return "foo";
}

unsigned int bar (unsigned int b)
{
  return UINT_MAX - b;
}

/* { dg-final { scan-assembler {\.section\t\.text\.foo[^\n]*\"0x20000006\"} } } */
/* { dg-final { scan-assembler {\.section\t\.text\.bar[^\n]*\"0x20000006\"} } } */

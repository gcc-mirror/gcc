/* { dg-do compile } */
/* { dg-skip-if "skip override" { *-*-* } { "-mfloat-abi=hard" } { "" } } */
/* { dg-options "-mpure-code -mcpu=cortex-m23 -march=armv8-m.base -mthumb -mfloat-abi=soft" } */

typedef int __attribute__ ((__vector_size__ (16))) V;

V v;

void
foo (void)
{
  v += (V){4095};
}

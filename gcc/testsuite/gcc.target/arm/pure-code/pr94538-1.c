/* { dg-do compile } */
/* { dg-require-effective-target arm_cpu_cortex_m23_ok } */
/* { dg-options "-mpure-code" } */
/* { dg-add-options arm_cpu_cortex_m23 } */

typedef int __attribute__ ((__vector_size__ (16))) V;

V v;

void
foo (void)
{
  v += (V){4095};
}

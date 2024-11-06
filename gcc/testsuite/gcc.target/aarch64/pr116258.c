/* { dg-do compile } */
/* { dg-options "-O2" } */

#pragma GCC target "+nosve"

#define vect16 __attribute__((vector_size(16)))
#define h(a) __builtin_assoc_barrier((a))

 vect16 float  f( vect16 float  x, vect16 float vconstants0)
{
  vect16 float  t = (x * (vconstants0[0]));
  return (x + h(t));
}

/* { dg-final { scan-assembler-times "\\\[0\\\]" 1 { target { aarch64_little_endian } } } } */
/* { dg-final { scan-assembler-times "\\\[3\\\]" 1 { target { aarch64_big_endian } } } } */
/* { dg-final { scan-assembler-not "dup\t" } } */
/* { dg-final { scan-assembler-not "ins\t" } } */

/* { dg-do run } */
/* { dg-require-effective-target s390_vx } */
/* { dg-options "-O3 -mzarch -march=arch13 -mzvector -std=gnu99 --save-temps" } */

#include <vecintrin.h>

/* The initial implementation of vec_sel used an IF_THEN_ELSE rtx.
   This did NOT match what the vsel instruction does.  vsel is a
   bit-wise operation.  Using IF_THEN_ELSE made the + operation to be
   simplified away in combine.  A plus operation affects other bits in
   the same element. Hence per-element simplifications are wrong for
   vsel.  */
vector unsigned char __attribute__((noinline))
foo (vector unsigned char a, vector unsigned char b, vector unsigned char c)
{
  return vec_sel (a + b, c, a);
}

/* FIXME: The OR operation still should be optimized away in that case.  */
vector unsigned char __attribute__((noinline))
bar (vector unsigned char a, vector unsigned char b, vector unsigned char c)
{
  return vec_sel (a | b, c, a);
}

int
main ()
{
  vector unsigned char v = (vector unsigned char){ 1 };

  if (foo (v, v, v)[0] != 3)
      __builtin_abort ();

  if (bar (v, v, v)[0] != 1)
    __builtin_abort ();

  return 0;
}

/* { dg-do run } */
/* { dg-require-effective-target riscv_v_ok } */
/* { dg-add-options riscv_v } */
/* { dg-additional-options "-std=gnu99" } */

typedef __attribute__((__vector_size__ (8))) unsigned char U;
typedef __attribute__((__vector_size__ (8))) signed long V;

signed char s;
V v;

void
__attribute__ ((noipa))
foo (U u, V *r)
{
  __builtin_memmove (&s, &u, 1);
  v += s;
  *r = v;
}

int
main ()
{
  V x;
  foo ((U) {248}, &x);
  if (x[0] != -8)
    __builtin_abort();
}

/* { dg-do compile } */
/* { dg-additional-options "-w -march=rv64gcv -mabi=lp64d" { target rv64 } } */
/* { dg-additional-options "-w -march=rv32gcv -mabi=ilp32" { target rv32 } } */

typedef __attribute__((__vector_size__ (4))) _Float16 F;
_Complex char cc;
F f;

void
foo ()
{
  __builtin_memmove (&f, &cc, 2);
  f *= f;
}

/* { dg-do "compile" { target { ! riscv_abi_e } } } */
/* { dg-options "-O2 -mcpu=tt-ascalon-d8" } */
_Float16 f;

void
foo ()
{
  f /= 3;
}

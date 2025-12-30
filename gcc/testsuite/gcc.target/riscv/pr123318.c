/* { dg-do compile { target rv64 } } */
/* { dg-options "-Ofast -mcmodel=medany -mexplicit-relocs -march=rv64gv" } */
typedef _Complex _Float16 CF;
_Complex int ci;
CF cf;

void
foo()
{
  ci += cf;
  ci -= (CF)0;
}

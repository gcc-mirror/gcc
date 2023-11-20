/* Check that int128 loads and stores are split.  */

/* { dg-do compile { target int128 } } */
/* { dg-options "-O3 -mzarch -march=zEC12" } */

__int128 global;

void f(__int128 x)
{
  global = x;
}

/* { dg-final { scan-assembler-times "lg\t" 2 } } */
/* { dg-final { scan-assembler-times "stg\t" 2 } } */

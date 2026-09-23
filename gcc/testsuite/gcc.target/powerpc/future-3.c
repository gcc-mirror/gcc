/* 32-bit doesn't generate vector pair instructions.  */
/* { dg-do compile { target lp64 } } */
/* { dg-options "-mdejagnu-cpu=future -O2" } */

/* Test to see that memcpy will use load/store vector pair with
   -mcpu=future.  */

#ifndef SIZE
#define SIZE 4
#endif

extern vector double to[SIZE], from[SIZE];

void
copy (void)
{
  __builtin_memcpy (to, from, sizeof (to));
  return;
}

/* { dg-final { scan-assembler {\mlxvpx?\M}  } } */
/* { dg-final { scan-assembler {\mstxvpx?\M} } } */

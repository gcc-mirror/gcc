/* { dg-do compile } */
/* { dg-options "-march=rv64gc_zbs -mabi=lp64" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Og" } } */

long long foo1 (long long a)
{
  return a | 0x1100;
}

long long foo2 (long long a)
{
  return a | 0x80000000000000ffull;
}

long long foo3 (long long a)
{
  return a | 0x8000000100000000ull;
}

long long foo4 (long long a)
{
  return a | 0xfff;
}

/* { dg-final { scan-assembler-times "bseti\t" 5 } } */
/* { dg-final { scan-assembler-times "ori\t" 3 } } */


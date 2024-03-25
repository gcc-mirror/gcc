/* { dg-do compile } */
/* { dg-options "-march=rv64gc_zbs -mabi=lp64" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Og" "-funroll-loops" } } */

int
foo(const long long B, int a)
{
  long long b = 1;    
  for (int sq = 0; sq < 64; sq++)
    if (B & (b << sq)) 
      a++;

  return a;
}

/* { dg-final { scan-assembler-times "bext\t" 1 } } */
/* { dg-final { scan-assembler-not {\mbset} } } */
/* { dg-final { scan-assembler-not {\msll} } } */

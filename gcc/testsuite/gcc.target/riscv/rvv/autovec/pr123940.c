/* { dg-do run } */
/* { dg-options "-O3 -march=rv64gcv -mabi=lp64d -std=gnu99 -fdump-tree-widening_mul" } */

long a;
long long b;
_Bool c[16];
char d[16];
char e = 0;
int f = 1;

int main ()
{
  for (long i = 0; i < 16; ++i)
    c[i] = 40;
  for (int j = 0; j < 16; j++)
    {
      e = (c[j] ? j : d[j]) + d[j];
      a = f * c[j] ?: ~0;
    }
  b = (int) e;
  if (b != 15)
    __builtin_abort ();
}

/* { dg-final { scan-tree-dump-not "FMA" "widening_mul" } } */

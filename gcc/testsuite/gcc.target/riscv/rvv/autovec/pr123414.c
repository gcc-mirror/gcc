/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvl256b -mabi=lp64d -mrvv-max-lmul=m8 -O3 -fsigned-char -fno-strict-aliasing -fwrapv -fdump-tree-optimized -std=gnu99" } */

signed char a=2;
long long b;
long c = 93;
int e[1][9];

void
g (long cc, int ee[][9])
{
  for (int i = 0; i < 4; i++)
    for (int j = 0; j < 5; j++)
      for (unsigned k = 0; k < 9; k++)
	{
	  a *= cc;
	  for (int l = 0; l < 6; l += (ee[k] <= 0) + 2)
	    ;
	}
}

int main() {
  g( c, e);
  b = (int)a;
  if (b != 34)
    __builtin_abort ();
}

/* { dg-final { scan-tree-dump-times "\[a-zA-Z_\]\[a-zA-Z0-9_\]+.=.VEC_PERM_EXPR <_\[0-9\]+, \\\{ 1(?:, 1){255} \\\}, \\\{ 0, 257, 258" 3 "optimized" } } */

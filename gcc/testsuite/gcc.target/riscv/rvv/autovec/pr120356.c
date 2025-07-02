/* { dg-do run } */
/* { dg-require-effective-target rvv_zvl256b_ok } */
/* { dg-options "-march=rv64gcv_zvl256b -mabi=lp64d -mrvv-vector-bits=zvl -O2" } */

unsigned char a = 5;
long long c[18];

static void d ()
{
  for (short i = 0; i < 60; i += 65413)
    for (char j = 0; j < 18; j++)
      {
	for (char k = 0; k < 18; k++)
	  a *= 143;
	for (char k = 0; k < 6; k++)
	  for (char l = 0; l < 18; l++)
	    c[l] = 0;
      }
}

int main ()
{
  d ();
  if (a + c[0] != 69)
    __builtin_abort ();
}

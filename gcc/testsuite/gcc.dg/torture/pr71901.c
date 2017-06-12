/* { dg-do compile } */
/* { dg-require-effective-target alloca } */

typedef struct { int _mp_size; } mpz_t[1];
int a, b;
void fn1()
{
  mpz_t c[1][b];
  for (;;) {
      int d = 0 >= 0 ? 0 == 0 ? c[0][0]->_mp_size ? -1 : 0 : 0 : 0,
	  e = 0 >= 0 ? 0 == 0 ? c[1][1]->_mp_size ? -1 : 0 : 0 : 0;
      if (d != e)
	a++;
  }
}

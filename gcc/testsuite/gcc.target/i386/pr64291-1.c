/* { dg-options "-O2" } */
/* { dg-additional-sources pr64291-2.c } */
/* { dg-do run { target lp64 } } */
void f(void*,...);
void g(void*,long,long);
int nnn=0;
long test=0;

typedef struct
{
  int _mp_size;
  unsigned long *_mp_d;
} __mpz_struct;
typedef __mpz_struct mpz_t[1];
void h(mpz_t);

int main ()
{
  mpz_t n, d;
  long nn, dn;
  unsigned long *np, *dup, *dnp, *qp;
  long alloc, itch;

  f (n);
  h (d);
  qp = (unsigned long*)__builtin_alloca(4099*8) + 1;
  dnp = (unsigned long*)__builtin_alloca (2049*8);
  alloc = 1;
  for (test = 0; test < 1; test++)
    {
      dn = d->_mp_size;
      dup = d->_mp_d;
      f (dnp, dup, dn);
      dnp[dn - 1] |= 1UL<<63;
      f (0);
      nn = nnn;
      np = n->_mp_d;
      qp[-1] = -757136820;
      qp[nn - dn + 1] = 14883681;
      f (0);
      if (dn >= 6)
	f (0);
      itch = nn + 1;
      if (itch + 1> alloc)
	{
	  g(0,alloc*8,(itch+1)*8);
	  alloc = itch + 1;
	}
      f (np, nn);
    }
  return 0;
}

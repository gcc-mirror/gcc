/* { dg-do run { target rs6000-*-linux* powerpc-*-linux*} } */
/* { dg-options -O2 } */

double dval = 0;

void splat (double d);

int main(void)
{
  splat(0);
  if (dval == 0)
    abort();
  exit (0);
}

void splat (double d)
{
  union {
    double f;
    unsigned int l[2];
  } u;
  
  u.f = d + d;
  u.l[1] |= 1;
  asm volatile ("stfd %0,dval@sdarel(13)" : : "f" (u.f));
}

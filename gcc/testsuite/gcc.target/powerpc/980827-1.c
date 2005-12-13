/* { dg-do run { target { *-*-linux* && ilp32 } } } */
/* { dg-options -O2 } */

extern void exit (int);
extern void abort (void);

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

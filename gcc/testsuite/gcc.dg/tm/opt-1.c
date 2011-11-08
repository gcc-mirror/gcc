/* { dg-do compile } */
/* { dg-options "-fgnu-tm -O" } */

extern void usleep (int) __attribute__((transaction_pure));
extern int rand(void) __attribute__((pure, transaction_pure));
extern int printf (const char *, ...);
extern void *malloc (__SIZE_TYPE__) __attribute__((malloc));
extern void xyzzy (void * (*)(void *));

typedef struct
{
  int id;
} parm;

int gvar;

void *hello(void *arg)
{
  parm *p=(parm *)arg;
  int tmp = p->id;
  int tmp3;
  printf ("Thread reads %d.\n", tmp);
  __transaction_atomic
    {
      int tmp2 = gvar;
      usleep ((int) (10.0*rand()/(10+1.0))/100);
      gvar = tmp + tmp2;
      tmp3 = gvar;
    }
  printf("tmp3 = %d\n", tmp3);
  return 0;
}

int
main()
{
  int i, n = rand();

  for (i=0; i<n; i++)
    xyzzy (hello);

  return 0;
}

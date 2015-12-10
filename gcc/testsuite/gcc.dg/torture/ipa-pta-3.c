/* { dg-do run } */
/* { dg-additional-options "-fipa-pta" } */

extern void abort (void);
extern void *malloc (__SIZE_TYPE__);

static int *p;
static void __attribute__((noinline,noclone)) foo ()
{
  p = (int *) malloc (24);
  *p = 2;
}
int main()
{
  foo ();
  if (*p != 2)
    abort ();
  return 0;
}

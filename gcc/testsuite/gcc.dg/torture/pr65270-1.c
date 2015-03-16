/* { dg-do run } */
/* { dg-options "-fno-common" { target hppa*-*-hpux* } } */

struct a 
{
  int a[100];
};
typedef struct a misaligned_t __attribute__ ((aligned (8)));
typedef struct a aligned_t __attribute__ ((aligned (32)));

__attribute__ ((used))
__attribute__ ((noinline))
void
t(void *a, int misaligned, aligned_t *d)
{
  int i,v;
  for (i=0;i<100;i++)
    {
      if (misaligned)
	v=((misaligned_t *)a)->a[i];
      else
	v=((aligned_t *)a)->a[i];
      d->a[i]+=v;
    }
}
struct b {int v; misaligned_t m;aligned_t aa;} b;
aligned_t d;
int
main()
{
  t(&b.m, 1, &d);
  return 0;
}

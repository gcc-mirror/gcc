/* { dg-do compile } */

struct a 
{
  int a[100];
};
typedef struct a b __attribute__ ((aligned (32)));
typedef struct a c __attribute__ ((aligned (4)));

int t(b *a)
{
  int i;
  c *ptr = a;
  for (i=0;i<100;i++)
    ptr->a[i]++;
}

/* The memory access is through a pointer of type c which means
   *ptr is not aligned.  */

/* { dg-final { scan-tree-dump "can't force alignment" "vect" } } */
/* { dg-final { scan-tree-dump-not "misalign = 0" "vect" } } */

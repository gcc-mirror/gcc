/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-pre" } */

typedef struct {
    unsigned int key;
} S;
typedef struct s1  {
    unsigned int key;
    unsigned int bits;
    struct s1 *left, *right;
}S1;
extern S a[1024];
static inline int bar( S* p, S1* n )
{
  S1 *curr;
  S1 *next;

  if ( n->left == n )
    return (int)(p->key == n->key);

  curr = n;
  next = n->left;

  while (curr->bits > next->bits ) {
      curr = next;
      if (p->key & (1 << curr->bits))
	next = curr->right;
      else
	next = curr->left;
  }

  return (int)(p->key == next->key);

}

int foo (S1 *root, int N)
{
  volatile int r;
  int i,j;
  for (i=0; i<N; i++)
    for (j=0;j<1024; j++)
      r = bar(&a[j], root);
  return 0;
} 

/* { dg-final { scan-tree-dump-times "key" 3 "pre" } } */

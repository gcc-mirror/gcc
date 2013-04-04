extern void abort (void);

typedef struct {
    int a[3];
    int num;
} t1;
t1 B[100];
int A[300];

void __attribute__((noinline,noclone))
bar (int *A, t1 *B, int n)
{
  int i;
  int *a = A;
  for (i=0; i<n; i++, a+=3)
    {
      a[0] = B[i].a[0];
      a[1] = B[i].a[1];
      a[2] = B[i].a[2];
    }
}

int main()
{
  int i;
  for (i=0; i<100; i++) 
    {
      B[i].num = i;
      B[i].a[0] = i * 3;
      B[i].a[1] = i * 3 + 1;
      B[i].a[2] = i * 3 + 2;
      __asm__ volatile ("");
    }
  bar (&A[0], &B[0], 100);
  for (i=0; i<300; i++)
    if (A[i] != i)
      abort ();
  return 0;
} 

/* { dg-final { cleanup-tree-dump "vect" } } */

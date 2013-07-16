/* { dg-do run } */

extern void abort(void);
extern int rand(void);

static void copy(int *r,int *a,int na)
{
  int i;
  for( i = 0 ; i < na ; i++ )
    r[i] = a[i];
}

static void foo(int *a,int na)
{
  int i;
  for( i = 0 ; i < na ; i++ )
    a[i] = rand();
}

static int cmp(int *a,int *b,int n)
{
  int i;
  for( i = 0 ; i < n ; i++ )
    if ( a[i] != b[i] )
      return -1;
  return 0;
}

void __attribute__((noinline,noclone))
test(int sz,int comm)
{
  int j,n;
  int v[64],w[64];
  for( j = 1 ; j <= sz ; j++ )
    {
      n = (2 * j - 1) * (2 * j - 1);
      foo(w,n);
      copy(v,w,n);
      if ( comm )
	if ( cmp(v,w,n) ) abort ();
    }
}

int main()
{
  test(2,1);
  return 0;
}

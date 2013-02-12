void __attribute__((noinline,noclone))
fn1 (int * __restrict f, int * __restrict d, unsigned short a, int c)
{
  unsigned short e;
  for (e = 0; e < a; ++e)
    f[e] = d[e * c];
}

extern void abort (void);

int main ()
{
  int a[32], b[3 * 32];
  int i, off;
  for (i = 0; i < 3 * 32; ++i)
    b[i] = i;
  for (off = 0; off < 8; ++off)
    {
      fn1 (&a[off], &b[off], 32 - off, 3);
      for (i = 0; i < 32 - off; ++i)
	if (a[off+i] != b[off+i*3])
	  abort ();
    }
  return 0;
}

/* { dg-final { cleanup-tree-dump "vect" } } */

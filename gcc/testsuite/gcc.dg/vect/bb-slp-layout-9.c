/* { dg-do compile } */
/* { dg-additional-options "-fno-tree-loop-vectorize" } */

int a[4], b[400], c[400], d[40000];

void f1()
{
  int a0 = a[0];
  int a1 = a[1];
  int a2 = a[2];
  int a3 = a[3];
  for (int i = 0; i < 100; ++i)
    {
      a0 ^= c[i * 4 + 0];
      a1 ^= c[i * 4 + 1];
      a2 ^= c[i * 4 + 2];
      a3 ^= c[i * 4 + 3];
      for (int j = 0; j < 100; ++j)
	{
	  a0 += d[i * 400 + j * 4 + 1];
	  a1 += d[i * 400 + j * 4 + 0];
	  a2 += d[i * 400 + j * 4 + 3];
	  a3 += d[i * 400 + j * 4 + 2];
	}
      b[i * 4 + 0] = a0;
      b[i * 4 + 1] = a1;
      b[i * 4 + 2] = a2;
      b[i * 4 + 3] = a3;
    }
  a[0] = a0;
  a[1] = a1;
  a[2] = a2;
  a[3] = a3;
}

/* { dg-final { scan-tree-dump-times "add new stmt: \[^\\n\\r\]* = VEC_PERM_EXPR" 2 "slp1" { target { vect_int && vect_perm } } } } */

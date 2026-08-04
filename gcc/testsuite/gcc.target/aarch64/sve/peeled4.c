/* { dg-do compile } */
/* { dg-options "-O3 -mautovec-preference=sve-only -msve-vector-bits=scalable -fdump-tree-vect-details" } */

char a[132];
char b[128];

unsigned __attribute__ ((noipa))
f (char x)
{
  unsigned ret = 0;
  for (int i = 1; i < 126; i += 2)
    {
      if (a[i - 1] > x || a[i + 2] > x)
	return 1;

      if (a[i + 4] > x)
	return 1;

      b[i] = x;
      b[i + 1] = x + 1;
    }
  return ret;
}

/* { dg-final { scan-tree-dump "vector alignment may not be reachable" "vect" } } */
/* { dg-final { scan-tree-dump-not "early break not supported: cannot peel for alignment" "vect" } } */
/* { dg-final { scan-tree-dump "LOOP VECTORIZED" "vect" } } */

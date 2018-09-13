/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "-O0" } { "" } } */
/* { dg-additional-options "-fdump-tree-fre1" } */

int x;
int main()
{
  x = 0;
  int z = x;
  int w = 1;
  for (int i = 0; i < 32; ++i)
    {
      if (z)
	w = 2;
      else
	w = 1;
      if (w == 2)
	__builtin_abort ();
    }
  return w;
}

/* { dg-final { scan-tree-dump-not "abort" "fre1" } } */

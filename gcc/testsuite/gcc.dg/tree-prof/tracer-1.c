/* { dg-options "-O2 -ftracer -fdump-tree-tracer" } */
volatile int a, b, c;
int main ()
{
  int i;
  for (i = 0; i < 1000; i++)
    {
      if (i % 17)
	a++;
      else
	b++;
      c++;
    }
  return 0;
}
/* Superblock formation should produce two copies of the increment of c */
/* { dg-final-generate { scan-tree-dump-times "c =" 2 "tracer" } } */
/* { dg-final-use { cleanup-tree-dump "tracer" } } */

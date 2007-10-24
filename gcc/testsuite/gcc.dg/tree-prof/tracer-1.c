/* { dg-options "-O2 -ftracer -fdump-tree-tracer" } */
main ()
{
  int i;
  int a, b, c;
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
/* { dg-final-generate { scan-tree-dump-times "goto <bb 6>;" 2 "tracer" } } */
/* { dg-final-use { cleanup-tree-dump "tracer" } } */

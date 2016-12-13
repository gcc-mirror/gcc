// { dg-do run }
// { dg-additional-options "-fdump-tree-asan0" }
/* { dg-skip-if "" { *-*-* } { "*" } { "-O0" } } */

int main(int argc, char **argv)
{
  int a = 123;

  if (argc == 0)
  {
    int *ptr;
    /* The label is not used in &label or goto label.  Thus '&a' should be
       marked just once.  */
    label:
      {
	ptr = &a;
        *ptr = 1;
	return 0;
      }
  }

  return 0;
}

/* { dg-final { scan-tree-dump-times "ASAN_MARK \\(UNPOISON, &a, 4\\);" 1 "asan0" } }  */

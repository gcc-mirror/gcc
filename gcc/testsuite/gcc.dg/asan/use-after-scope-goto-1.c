// { dg-do run }
// { dg-additional-options "-fdump-tree-asan0" }
/* { dg-skip-if "" { *-*-* } { "*" } { "-O0" } } */

int main(int argc, char **argv)
{
  int a = 123;
  int b = 123;
  int c = 123;
  int d = 123;
  int e = 123;
  int f = 123;

  if (argc == 0)
  {
    int *ptr;
    int *ptr2;
    int *ptr3;
    int *ptr4;
    int *ptr5;
    int *ptr6;
    label:
      {
	ptr = &a;
        *ptr = 1;
	ptr2 = &b;
        *ptr2 = 1;
	ptr3 = &c;
        *ptr3 = 1;
	ptr4 = &d;
        *ptr4 = 1;
	ptr5 = &e;
        *ptr5 = 1;
	ptr6 = &f;
        *ptr6 = 1;
	return 0;
      }
  }
  else
    goto label;

  return 0;
}

/* { dg-final { scan-tree-dump-times "ASAN_MARK \\(UNPOISON, &a, 4\\);" 2 "asan0" } }  */
/* { dg-final { scan-tree-dump-times "ASAN_MARK \\(UNPOISON, &c, 4\\);" 2 "asan0" } }  */
/* { dg-final { scan-tree-dump-times "ASAN_MARK \\(UNPOISON, &e, 4\\);" 2 "asan0" } }  */

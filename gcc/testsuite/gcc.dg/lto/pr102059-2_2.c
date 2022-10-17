extern int foo2 (int *b);

#pragma GCC target "cpu=power10"
int
main (int *a)
{
  *a = foo2 (a);
  return 0;
}


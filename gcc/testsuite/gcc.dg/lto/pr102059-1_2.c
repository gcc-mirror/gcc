extern int foo2 (int *b);

#pragma GCC target "cpu=power10"
__attribute__ ((always_inline))
int
main (int *a)
{
  *a = foo2 (a);
  return 0;
}


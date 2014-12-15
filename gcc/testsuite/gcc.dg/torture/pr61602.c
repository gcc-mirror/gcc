int a;
int *b = &a, **c = &b;
int
main ()
{
  int **d = &b;
  *d = 0;
}

void exit (int);

int
sub (int i, int array[i++])
{
  return i;
}

int
main(void)
{
  int array[10];
  exit (sub (10, array) != 11);
}

struct foo
{
  int a, b, c;
};

foo (struct foo *a)
{
  a[0] = a[1];
}

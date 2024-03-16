struct foo
{
  int a, b, c;
};

void
foo (struct foo *a)
{
  a[0] = a[1];
}

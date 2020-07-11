struct foo
{
  int *v;
};

int test (void)
{
  struct foo f = {};
  return *f.v;
}

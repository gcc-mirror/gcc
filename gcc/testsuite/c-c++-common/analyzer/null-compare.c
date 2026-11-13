int
test_int_pointer (int *p)
{
  return p == 0;
}

int
test_void_pointer (void *p)
{
  return p == 0;
}

struct s;

int
test_struct_pointer (struct s *p)
{
  return p == 0;
}

int
test_function_pointer (int (*fp) (void))
{
  return fp == 0;
}

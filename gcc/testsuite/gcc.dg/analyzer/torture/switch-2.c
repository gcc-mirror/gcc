struct s
{
  int f0;
  int f1;
};

int test (int cmd)
{
  int err = 0;
  struct s foo;
  struct s bar;
  
  switch (cmd)
    {
    case 0:
      foo.f0 = 0;
      break;
    case 1:
      foo.f0 = 1;
      break;
    case 30 ... 50:
    case 70 ... 80:
      __builtin_memset (&bar, 0, sizeof (bar));
      break;      
    }

  switch (cmd)
    {
    default:
      return -1;
    case 0 ... 1:
      return foo.f0;
      break;
    case 42:
      return bar.f1;
      break;
    case 65:
      return bar.f1;
      break;
    }
  return err;
}

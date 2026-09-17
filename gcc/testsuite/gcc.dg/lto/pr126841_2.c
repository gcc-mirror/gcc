static int
same_fn (int x)
{
  return x * 33 + 7;
}

int (*c_callback) (int) = same_fn;

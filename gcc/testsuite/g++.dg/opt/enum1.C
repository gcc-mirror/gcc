// Verify that we don't confuse precision and mode for enums.
// { dg-do run }
// { dg-options "-O" }

extern "C" void abort();

enum E {
  zero = 0, 
  test = 0xbb
};

static bool foo(unsigned char *x)
{
  E e = static_cast<E>(*x);
  switch (e)
    {
    case test:
      return true;
    default:
      return false;
    }
}

int main()
{
  unsigned char dummy = test;
  if (! foo(&dummy))
    abort ();
  return 0;
}

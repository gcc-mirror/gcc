/* Test for ICE in predictive commoning with empty loop header block
   on arm-none-linux-*.  */

#pragma GCC diagnostic ignored "-Wreturn-type"

struct Foo
{
  double *ptr;
  
  Foo (double *ptr_)
    : ptr (ptr_)
  {
  }
  
  Foo PostInc ()
  {
    return Foo (ptr++);
  }
};

bool Baz (Foo first, double *last)
{
  Foo prev (first.ptr);
  
  first.ptr++;

  while (first.ptr != last)
    if (*first.PostInc ().ptr < *prev.PostInc ().ptr)
      return false;
}


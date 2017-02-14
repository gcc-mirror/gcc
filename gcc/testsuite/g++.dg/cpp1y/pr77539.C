// PR c++/77539
// { dg-do compile { target c++14 } }

constexpr int foobar()
{
  int array[100] = {};
  int *ar = array;
  if (ar == nullptr) // Error...
  {
    return 0;
  }
  return 1;
}
static_assert(foobar(),"");

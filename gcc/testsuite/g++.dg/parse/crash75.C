// PR c++/108438
// { dg-options "-Wlogical-not-parentheses" }

template <typename T>
T foo (T arg, T& ref, T* ptr)
{
  int a = 1;
  return static_cast<T!>(a); // { dg-error "expected" }
}

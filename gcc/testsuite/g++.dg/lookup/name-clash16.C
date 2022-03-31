typedef int T;

struct A
{
  template <class T>
  struct B
  {
    T t;
  };

  // OK, earlier T was found in template header, didn't look in A.
  typedef float T;
};

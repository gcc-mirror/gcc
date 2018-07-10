template <class T>
struct A
{
  template <class U>
  struct B
  {
    friend struct C;
  };
};

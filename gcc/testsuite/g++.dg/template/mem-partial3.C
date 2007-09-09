// PR c++/33342

template <bool B, class T = void>
struct enable_if_c {
  typedef T type;
};

template <class T>
struct A
{
  template <class U, class V>
  struct B;

  template <class U>
  struct B<U, typename enable_if_c<U::sub::value==0>::type>
  { };
};

struct C
{
  struct sub
  {
    static const int value = 0;
  };
};

    
A<int> a;
A<int>::B<C, void> b;

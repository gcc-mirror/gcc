// PR c++/14122

extern const char str[];

template <const char* P>
struct A
{
  template <const char* R>  void foo();
};

template class A<str>;

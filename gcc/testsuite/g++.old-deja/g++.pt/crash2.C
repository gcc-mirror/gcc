// Build don't link:

template <class T> 
struct S1
{
  T* t;
  static int foo;
};


struct S2 : public S1<S2>
{
  S2* s;
};

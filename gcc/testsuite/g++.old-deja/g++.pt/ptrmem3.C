// { dg-do assemble  }

template <class T>
struct S : public S<T*> {};
template <>
struct S<int**> {};

void g()
{
  int S<int*>::*p;
  int S<int>::*q = p;
}

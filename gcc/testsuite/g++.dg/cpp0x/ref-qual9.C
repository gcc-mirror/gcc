// PR c++/57064
// { dg-require-effective-target c++11 }

template <class T> T&& move(T& t);

struct A {
  void p() &;
  int p() &&;
};

void g(A &&a)
{
  int i = move(a).p();
}

// { dg-do assemble  }

void h(int);

template <class T>
class i {};

struct B
{
  int i;
};

template <class T>
struct D : public B
{
  void f();
  void g() { h(i); }
};

template <class T>
void D<T>::f()
{
  h(i);
}

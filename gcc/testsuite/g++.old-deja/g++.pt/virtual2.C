// { dg-do assemble  }
// Origin: Mark Mitchell <mark@codesourcery.com>

template <class T>
struct B 
{
  virtual void f() = 0;
};

template <class T>
struct D : public B<T> {
  virtual void f();
};

void g() {
  B<int>* bi = new D<int>;
}

template <class T>
void B<T>::f() {}


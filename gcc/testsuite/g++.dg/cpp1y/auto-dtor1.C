// DR 1586
// { dg-do run { target c++1y } }

template <class T>
void f (T* p)
{
  p->~auto();
}

int d;
struct A { ~A() { ++d; } };

int main()
{
  f(new int(42));
  f(new A);
  if (d != 1)
    throw;

  (new int)->~auto();
}

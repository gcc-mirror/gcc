// DR 1586
// { dg-options "-std=c++1y" }
// { dg-do run }

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

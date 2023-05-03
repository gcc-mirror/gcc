// PR c++/109649

template <typename>
class X
{
  void f(){}
};

class Y
{
  friend void X<int>::f();	// { dg-error "private" }
};

int main()
{
  X<int> t;
  t.f();			// { dg-error "private" }
  Y b;
}

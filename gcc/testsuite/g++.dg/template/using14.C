// PR c++/26102

template <class T> struct B1 { int i(); };

struct B2 { int i(); };

template <class T> struct C : public B1<T>, public B2
{
  using B2::i;
  void f()
  {
    i();			// should be accepted
    i.i();			// { dg-error "member" }
  }
};

int main()
{
  C<int> c;
  c.f();			// { dg-message "required" }
}

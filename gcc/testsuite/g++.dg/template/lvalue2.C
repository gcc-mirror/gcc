// PR c++/50835

struct A {};

struct B
{
  explicit B(A &);
  operator A&() const;
};

void should_be_lvalue(A&);

template <typename>
void f()
{
    A v;
    should_be_lvalue(true ? B(v) : v);
}

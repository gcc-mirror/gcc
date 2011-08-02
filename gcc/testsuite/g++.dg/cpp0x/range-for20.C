// PR c++/49834
// { dg-options -std=c++0x }

struct A
{
  template <typename T> T get_value() const;
};

struct B {
  A first, second;
};

struct C
{
  B* begin() const;
  B* end() const;
};

template <typename Ret>
Ret f(const C &p)
{
  for (const B &i: p)		// OK
    i.second.get_value<int>();
  for (const auto &i: p)	// ERROR
    i.second.get_value<int>();
  return Ret(0);
}

void g()
{
  f<int>(C());
}

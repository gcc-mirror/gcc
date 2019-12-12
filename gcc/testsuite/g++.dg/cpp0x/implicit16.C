// PR c++/89381
// { dg-do compile { target c++11 } }

template<typename T>
struct base
{
  base() { }
  base(const base&) { }
  base(base&&) { }
  base& operator=(const base&) { return *this; }
  base& operator=(base&&) { return *this; }
};

struct foo : base<int>
{
    using base<int>::base;
    using base<int>::operator=;
};

//using workaround = decltype(foo{*static_cast<foo const*>(0)});

struct bar
{
    bar& operator=(foo ve)
    {
        value = ve;
        return *this;
    }

    foo value;
};

int main()
{
    foo a;
    foo b{a};
}

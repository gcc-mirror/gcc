// PR c++/92145
// { dg-do compile { target c++11 } }
// { dg-additional-options "-Wdeprecated-copy" }

struct base
{
  base() { }
  base(const base&) { }
  base(base&&) { }
  base& operator=(const base&) { return *this; }
  base& operator=(base&&) { return *this; }
};

struct foo : base
{
  //using base::base;
  using base::operator=;
};

struct bar
{
  bar& operator=(foo v)
  {
    value = v;
    return *this;
  }

  foo value;
};

int main()
{
  foo a;
  foo{a};
}

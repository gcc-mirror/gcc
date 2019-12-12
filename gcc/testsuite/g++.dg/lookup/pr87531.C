// PR c+/87531 lookup of operator= in templates
// { dg-do run }

struct Base {
  void operator= (Base const&);
};

void Base::operator= (Base const &)
{
}

template <typename T>
struct Derived : Base
{
  T v;

  Derived() : v (0) {}
  Derived(T v_) : v (v_) {}

  T &assign1 (Derived const& rhs)
  {
    operator=(rhs); // erroneously bound to Base::operator=
    return v;
  }

  T &assign2 (Derived const& rhs)
  {
    this->operator=(rhs); // erroneously bound to Base::operator=
    return v;
  }
};

template <typename T>
struct Single
{
  T v;

  Single () : v (0) {}
  Single (T v_) : v (v_) {}

  T &assign1 (Single const& rhs)
  {
    operator=(rhs); // lookup failed
    return v;
  }

  T &assign2 (Single const& rhs)
  {
    this->operator=(rhs); // Marked as dependent, happened to work
    return v;
  }
};

int main()
{
  Derived<int> a, b(123);

  if (a.assign1 (b) != 123)
    return 1;

  if (a.assign2 (b) != 123)
    return 2;

  Single<int> c, d(123);
  
  if (c.assign1 (d) != 123)
    return 3;

  if (c.assign2 (d) != 123)
    return 4;

  return 0;
}

template <class T> struct static_abort {};

template <class E>
struct any
{
  const E& self() const { return static_cast<const E&>(*this); }
};

struct range : public any<range>
{
  range() {}

  template <class U>
  range(const U&)
  {
    typedef typename static_abort<U>::ret t;
  }
};

int main()
{
  const any<range>& r = *new range();
  r.self();
}

// PR c++/98206
// { dg-do run }
// { dg-options "-fsanitize=alignment -std=c++11 -fno-sanitize-recover=alignment" }

template <typename Derived>
struct Base1
{
  char c1;
};

template <typename Derived>
struct Base2
{
  char c2;
  const Derived &get2 () const { return static_cast<const Derived &> (*this); }
};

struct X : public Base1<X>, public Base2<X>
{
  X (const char *d) : data{d} {}
  const char *data;
};

int
main ()
{
  X x = X{"cheesecake"};
  const char *p = x.get2 ().data;
  if (p[0] != 'c')
    __builtin_abort ();
}

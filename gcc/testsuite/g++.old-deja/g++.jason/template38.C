// Special g++ Options: -fguiding-decls

struct A {
  friend int operator== (const A&, const A&);
  A (int) { }
};

template <class T> int
operator== (const T&, const T&)
{
  return 0;
}

main ()
{
  A a (1);
  return a == 1;
}

// Build don't link:

template <int B>
class foo;

template <class U>
class bar
{
  typedef foo<(U::id > 0)> foobar;
};

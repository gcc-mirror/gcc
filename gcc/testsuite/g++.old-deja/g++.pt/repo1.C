// Bug: g++ complains about duplicate explicit instantiations with -frepo.
// From Jason Merrill <jason@cygnus.com>

// Build then link:
// Special g++ Options: -frepo

template <class T> struct A {
  virtual ~A () { }
};

template <class T> void g (T t) { }

template class A<int>;

int main ()
{
  g (42);
}

// The -pedantic option must be omitted to trigger the crash.
// { dg-do compile }
// { dg-options "" }

// PR c++/7982: Crash warning about implicit typename.
// The base class refers to another typename, while the
// name lookup finds a template.

template <typename T> struct X {};

template <typename T> struct C {
  typedef typename T::X X;
};

template <typename T> struct A : public C<T> {
  typedef X<int> X; // { dg-warning "lookup|dependent base|typename" }
};

// Verify we don't care about the access specifier when declaring
// a partial specialization of a member class template.

struct A1 {
  template<class T> struct B { };
private:
  template<class T> struct B<T*> { }; // { dg-bogus "different access" }
};

struct A2 {
  template<class T> struct B { };
  template<class T> struct B<T*>;
private:
  template<class T> struct B<T*> { }; // { dg-bogus "different access" }
};

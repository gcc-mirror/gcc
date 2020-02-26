// PR c++/87748
// { dg-do compile { target c++11 } }

template <class T> T&& declval();
template <bool B, class T> struct enable_if;
template <class T> struct enable_if <true, T> { typedef T type; };
struct true_type { static const bool value = true; };
struct false_type { static const bool value = false; };

struct string
{
  string (const char *p);
};

/// Template to map all type arguments to void, useful for SFINAE, see also WG21 N3911.
template<class...> using void_t = void;

/// REQUIRES<value> - Simplified version of enable_if<> to use SFINAE in function templates.
template<bool value> using REQUIRES = typename ::enable_if<value, bool>::type;

/// DerivesString<T> - Check if @a T is of type 'string'.
template<class T> struct DerivesString
{
  static const int value = __is_same_as (T, string);
};

/// Has__aida_visit__<T,Visitor> - Check if @a T provides a member template __aida_visit__<>(Visitor).
template<class, class, class = void> struct Has__aida_visit__ : false_type {};
template<class T, class V>
struct Has__aida_visit__<T, V, void_t< decltype (declval<T>().template __aida_visit__<V> (*(V*) 0)) >> : true_type {};

struct Foo {
  template<class V> void __accept__ (V*);
};

/// Base template for Visitor classes, dispatches operator() to visit_<type>() methods.
template<class DerivedVisitor>
class VisitorDispatcher {
  DerivedVisitor* derived () { return static_cast<DerivedVisitor*> (this); }
protected:
  typedef const char* Name; ///< Member name argument type for visit() methods.
public:

  // dispatch for calls like: visitor (field, "field");

  template<class A,
           REQUIRES< (!Has__aida_visit__<A, DerivedVisitor>::value) > = true>
  void operator() (A &a, Name n)
  { return derived()->visit_string (a, n); }
};

class PspecVisitor : public VisitorDispatcher<PspecVisitor> {
public:
  void visit_string (::string &a, Name name);
};

int
main (int argc, char *argv[])
{
#ifdef WITHASSERT // define this to fix g++-8.1.1
  static_assert (Has__aida_visit__<string, PspecVisitor>::value == false, "");
#endif
  PspecVisitor v;
  string str ("A string");
  v (str, "some_string");
  static_assert (Has__aida_visit__<string, PspecVisitor>::value == false, "");
  return 0;
}
// g++ -std=gnu++14 -Wall -O2 aidavisit.cc && ./a.out

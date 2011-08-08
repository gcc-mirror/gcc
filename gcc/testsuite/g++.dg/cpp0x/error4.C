// PR c++/49156
// { dg-options -std=c++0x }

template<typename T> T declval();

template<typename T>
struct S {

  template<typename U>
    static U get(const volatile T&);

  template<typename U>
    static decltype(*declval<U>()) get(...); // { dg-error "operator*" }

  typedef decltype(get<T>(declval<T>())) type; // { dg-error "no match" }
};

struct X { };

S<X>::type x;

// { dg-prune-output "note" }

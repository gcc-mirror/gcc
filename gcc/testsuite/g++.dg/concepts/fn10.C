// { dg-do compile { target c++17_only } }
// { dg-options "-fconcepts-ts" }

// Test that constraint satisfaction checks work even when
// processing template declarations.

namespace std
{

struct ostream { };
ostream cout;

template<typename T>
auto begin(T& t) -> decltype(t.begin()) { return t.begin(); }

template<typename T>
auto begin(T const& t) -> decltype(t.begin()) { return t.begin(); }

template<typename T>
auto end(T& t) -> decltype(t.end()) { return t.end(); }

template<typename T>
auto end(T const& t) -> decltype(t.end()) { return t.end(); }

} // namespace std


template <typename T>
  concept bool Float()
  {
    return __is_same_as( T, float );
  }

template <typename T>
  constexpr decltype(auto) project( T t )
  {
    return t;
  }

template <typename T>
  concept bool Concept()
  {
    return requires( T t ) { // { dg-message "in requirements" }
      requires Float<decltype( project(t) )>();
    };
  }

template <Concept E, Concept F>
  constexpr decltype(auto) operator<<( E&& e, F&& f ) {}

template <Concept T>
  void foo( T t )
  {
    // Try to resolve operator<< from within a template context but
    // with non-dependent arguments. We need to ensure that template
    // processing is turned off whenever checking for satisfaction.
    std::cout << "OK"; // { dg-error "no match" }
  }


template <typename R>
concept bool Range()
{
  return requires( R r ) {
    requires __is_same_as(
      decltype(std::begin(r)), decltype(std::end(r)) );
  };
}

struct A
{
  A() = default;
  A( const A& ) = default;

  // Derivation from this class forces the instantiation of
  // this constructor, which results in the __is_same_as type
  // trait above to become error_mark_node in this declaration.
  template <Range R>
    explicit A( R&& r ) { }
};

struct C : A
{
  C() = default;
  C( const C& ) = default;
};

int main()
{
  C c; // OK
  return 0;
}

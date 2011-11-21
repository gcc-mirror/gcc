// PR c++/48468
// { dg-options -std=c++0x }
// { dg-prune-output "note" }

template<class T>
T&& declval() noexcept;

template< class T >
inline void f1( T& x ) noexcept( noexcept( declval<T&>().foo() ) ) // { dg-error "Z" }
{
  x.foo();
}

template< class T,
  bool Noexcept = noexcept( declval<T&>().foo() ) // { dg-error "no member|not convert" }

>
inline void f2( T& x ) noexcept( Noexcept )
{
  x.foo();
}

// a common and trivial mistake
template< class T >
inline void f3( T& x ) noexcept( declval<T&>().foo() ) // { dg-error "Z" }
{
  x.foo();
}

struct X
{
  void foo();
};

struct Y
{
  void foo() noexcept;
};

struct Z {};

int main()
{
  X x; Y y; Z z;

  static_assert( !noexcept( f1(x) ), "OK." );
  static_assert( !noexcept( f2(x) ), "OK." );
  // static_assert( !noexcept( f3(x) ), "shall be ill-formed(OK)." );

  static_assert(  noexcept( f1(y) ), "OK." );
  static_assert(  noexcept( f2(y) ), "OK." );
  // static_assert(  noexcept( f3(y) ), "shall be ill-formed(OK)." );

  noexcept( f1(z) );		// { dg-message "required" }
  static_assert(  noexcept( f2(z) ), "shall be ill-formed." ); // { dg-error "no match" }
  noexcept( f3(z) );		// { dg-message "required" }
}

// PR c++/26988

struct B{};

struct Bar : virtual B {               
  template <typename T> Bar( T const& cast );
};

template <> Bar::Bar( int const & cast ) {}

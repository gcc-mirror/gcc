// PR c++/57429
// { dg-do compile { target c++11 } }

void f() = delete;

template< typename t >
void ft() { f( t() ); }

template< typename t >
struct ct { decltype( f( t() ) ) m; };

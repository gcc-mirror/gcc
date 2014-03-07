// PR c++/48313
// { dg-do compile { target c++11 } }

template<typename F>
void f(F&&) { }

void g() { }

template<typename T> void h() { }

int main()
{
  f( g );       // OK
  void (&p)() = h<int>;
  f( p );       // OK
  f( h<int> );  // ???
}


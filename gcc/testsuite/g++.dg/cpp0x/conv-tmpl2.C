// PR c++/92031 - bogus taking address of rvalue error.
// { dg-do compile { target c++11 } }

struct x { const int& l; };

void a(const x&) {}

template<class E>                          
void f() {
  a(x { 0 });
}

void g() {
  a(x { 0 });
}

void
test ()
{
  f<int>();
}

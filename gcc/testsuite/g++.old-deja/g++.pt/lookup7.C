// { dg-do assemble  }
// Origin: Jason Merrill <jason@cygnus.com>

struct A { typedef int B; };
template <int B> struct C : public A { 
  B b; 
  void f();
};


template <int B>
void C<B>::f() {
  B b;
}

// { dg-do run  }
template <class T> struct A {
  template <class U> struct B {
    template <class V> struct C {
      template <class W> struct D {
	template <class X> struct E {
	  template <class Y> struct F {
	    template <class Z> void f (Z) { }
	    void g () { }
	  };
	};
      };
    };
  };
};

int main ()
{
  A<int>::B<int>::C<int>::D<int>::E<int>::F<int> b;
  b.f (42);
  b.g ();
}

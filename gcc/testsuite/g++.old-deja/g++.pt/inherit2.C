// Test that we warn about unqualified references to implicit typenames.
// Bug: g++ is confused by the binding for ::AN and crashes.
// Special g++ Options:
// excess errors test - XFAIL *-*-*

template <class T> struct A {
  struct AA { };
  struct AB { };
  struct AC { };
};

template <class T> struct B: public A<T> {
  friend struct B::AA;		// OK
  friend AB;			// WARNING - needs class-key
  friend struct AC;		// WARNING - refers to ::AC
};

B<int> b;

int main () { }

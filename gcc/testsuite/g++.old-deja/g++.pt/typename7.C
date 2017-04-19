// { dg-do assemble  }
// { dg-options "" }
// Test for implicit typename

template <class T>
struct A {
protected:
  typedef struct B { } B;
};

template <class T>
struct C { };

template <class T>
struct D : public A <C <T> > {
  void f ()
    {
      B* new_entries = (B *) 0;  // { dg-error "'B' was not declared in this scope" "B" }
      // { dg-error "'new_entries' was not declared in this scope" "new_entries" { target *-*-* } .-1 }
      // { dg-error "expected" "exp" { target *-*-* } .-2 }
    }
};

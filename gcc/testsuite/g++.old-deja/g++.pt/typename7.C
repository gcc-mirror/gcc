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
      B* new_entries = (B *) 0;  // { dg-warning "" } implicit typename
    }
};

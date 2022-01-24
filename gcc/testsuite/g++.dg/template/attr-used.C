// { dg-do compile }

template<typename T> struct X {
  void g () {}
  void f () __attribute__((__used__)) {}
};

extern X<int> x; // X<int> incomplete here

template <typename T>
void Frob (T t) {
  x.g(); // X<int> completed here, X<int>::f's body marked for instantiation
}

// Make sure X<int>::f is emitted
// { dg-final { scan-assembler "_ZN1XIiE1fEv:" } }

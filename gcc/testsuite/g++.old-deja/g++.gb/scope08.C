// Build don't link: 
// GROUPS passed gb scope
template <class X> class C {
public:
  int f (X ob) { return 0; }
  int g (X ob) { return f (ob); }
};

class D {
public:
  class E { public: E (); };

  C <E> x;
};

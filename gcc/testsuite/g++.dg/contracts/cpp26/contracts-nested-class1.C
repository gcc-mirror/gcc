// { dg-do compile { target c++23 } }
// { dg-additional-options "-fcontracts" }

void gfn3(int n) pre ( n > 0 );

struct Outer {
  struct Inner {
    void fn(int n) pre ( n > 0 && bob > 1 );
    void fn2(int n) pre ( n > 0 && bob > 1 );
  };

  void fn(int m) pre (m > 1 );
  friend void Inner::fn(int n) pre ( n > 0 && bob > 1 ); // { dg-error "not declared" }

  friend void gfn(int p) pre ( p > 0 );
  friend void gfn(int q) pre ( q > 1 );

  friend void gfn2(int q);
  friend void gfn2(int p) pre ( p > 0 ) { } // { dg-error "declaration adds contracts" }

  static int bob;
};
int Outer::bob{-1};

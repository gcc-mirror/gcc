// { dg-do run }
// { dg-options "-std=c++2a -fcontracts -fcontract-continuation-mode=on" }
// { dg-skip-if "requires hosted libstdc++ for stdc++exp" { ! hostedlib } }

void gfn3(int n) [[ pre: n > 0 ]];

struct Outer {
  struct Inner {
    void fn(int n) [[ pre: n > 0 && bob > 1 ]];
  };

  void fn(int m) [[ pre: m > 1 ]];

  friend void gfn1(int q);
  friend void gfn1(int p) [[ pre: p > 0 ]] { }

  friend void gfn2(int q, Outer *);
  friend void gfn2(int p, Outer *) [[ pre: p > 0 ]] { }

  friend void gfn3(int n);

  static int bob;
};
int Outer::bob{-1};

void Outer::Inner::fn(int x) { }
void Outer::fn(int y) { }

void gfn3(int n) { }
void gfn1(int q);

int main(int, char **) {
  Outer::Inner in;
  in.fn(-5);
  Outer out;
  out.fn(-6);
  gfn1(-7);
  gfn2(-8, &out);
  gfn3(-9);
}


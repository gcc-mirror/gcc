// check that we do not ICE with an empty nontrivial parameter
// { dg-do run { target c++23 } }
// { dg-additional-options "-fcontracts -fcontract-checks-outlined" }

struct NTClass {
  NTClass(){};
  NTClass(const NTClass&){}
  ~NTClass(){};
};

struct Empty {};

void f (const NTClass i) pre (true){
}

void g (const Empty i) pre (true){
}


struct S {
  void f (const NTClass i)
    post ( true);

  void g (const Empty i)
    post ( true);

};

void
S::f (NTClass i){}

void
S::g (Empty i){}

int main(){

  NTClass n;
  f(n);
  g(Empty{});


}

// check that we do not have conflicts with post condition that includes a return value
// identifier and post condition on an identically named void function with the
// additional parameter that matches the returen value of the first function
// { dg-do run { target c++23 } }
// { dg-additional-options "-fcontracts -fcontract-evaluation-semantic=observe " }


int f() post(r:r>1){ return 2;}
void f(int i) post(true){ }

int main() {

  f();
  f(2);
}

// check that we do not crash when capturing a constified entity in a contract assertion lambda
// { dg-do compile { target c++23 } }
// { dg-additional-options "-fcontracts -fcontract-evaluation-semantic=observe " }
void f(int i, int j) pre( [i, &j](){ return true;} ( ))
{}

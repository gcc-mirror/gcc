// PR c++/101232
// { dg-do compile { target c++11 } }

struct X {
    int a;
    int b;
};

void f() {
    auto x = X{ 1, {2 };	// { dg-error "expected.*before" } 
}

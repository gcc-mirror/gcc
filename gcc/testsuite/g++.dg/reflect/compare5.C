// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test splicing of explicit object member functions.

struct Y {
    int g(this Y const&, int, int);
};
static_assert(&Y::g == &[:^^Y::g:]);

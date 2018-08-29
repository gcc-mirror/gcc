// PR c++/82336
// { dg-do link { target c++11 } }

struct foo { int x = 5; };
struct bar : foo { bar() = default; };
struct baz { bar x; };
void qux(baz = {}){}
int main() { qux(); }

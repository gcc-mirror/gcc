// PR c++/12218
// { dg-do run }

struct C { int i, j; };
typedef int C::*mPtr;
extern const mPtr should_be_0 = &C::i;
extern const mPtr should_be_4 = &C::j;

int main () {
}

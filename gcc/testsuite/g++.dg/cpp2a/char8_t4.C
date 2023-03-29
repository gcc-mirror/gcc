// PR c++/106656 - P2513 - char8_t Compatibility and Portability Fixes
// { dg-do compile { target c++20 } }
// [diff.cpp20.dcl]

struct A {
	char8_t s[10];
};
struct B {
	char s[10];
};

void f(A);
void f(B);

int main() {
	f({u8""}); // { dg-error "ambiguous" }
}

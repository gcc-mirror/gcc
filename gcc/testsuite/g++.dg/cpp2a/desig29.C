// PR c++/110114
// { dg-do compile { target c++20 } }

struct B;

void foo(const B &) {}

int main() {
    foo({.a=0}); // { dg-error "invalid" }
}

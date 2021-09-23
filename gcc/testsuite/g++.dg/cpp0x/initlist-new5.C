// PR c++/99643
// { dg-do compile { target c++11 } }

struct Foo {};
Foo get_foo();

int main() {
    new Foo[1]{get_foo()};
}

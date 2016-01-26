// PR c++/68949
// { dg-do run { target c++11 } }

struct Sub {
    int i;

    constexpr Sub() : i(-1) {} // remove constexpr and it works as expected
    Sub(Sub&& rhs); // remove this constructor and it works as epxected.
};

// v-- move this inline and it works as expected
// v-- remove ': Sub()' and it works as expected
Sub::Sub(Sub&& rhs) : Sub() { int tmp = i; i = rhs.i; rhs.i = tmp; }

struct Class {
    // v-- remove '[1]' and it works as expected
    // v-- add '= {}' and it works as expected
    Sub s[1];

    // v-- add ': s{}' and it works as expected
    // v-- removing this constructor makes it work as expected
    Class() {}
};

int main() {
    Class c;
    if (c.s[0].i != -1)
      __builtin_abort();
}

// PR c++/69078
// { dg-do run { target c++14 } }
// { dg-options "-Wall" }

#include <cassert>

struct Class {
    Class(void (*_param)()) : data(_param) {}
    void (*data)();
};

void funUser(void (*test)(int)) {
    test(60);
}

void user(Class& c, int i) {
    (void)i;
    assert (c.data);
}

void probe() {}

int main() {
    static Class instance = { probe };
    funUser([](auto... p) {
        user(instance, p...);
    });
}

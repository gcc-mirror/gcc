// PR c++/59465
// { dg-do compile }

struct I {
    const bool b;
};
struct O {
    I a[2];
    static I const data[2];
    O() : a(data){}  // { dg-error "invalid initializer for array member" }
};

I const O::data[2] = {true, false};

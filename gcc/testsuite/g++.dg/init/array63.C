// PR c++/59465
// { dg-do compile }

struct I {
    const bool b;
};
struct O {
    I a[2];
    static I const data[2];
    O() : a(data){}  // { dg-error "array must be initialized" }
};

I const O::data[2] = {true, false};

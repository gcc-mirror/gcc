// PR c++/47067
// { dg-do compile { target c++11 } }

struct X {
    virtual void x();
    virtual ~X();
};

struct Y {
    virtual void y();
    virtual ~Y();
};

struct Z: X, Y {} z;

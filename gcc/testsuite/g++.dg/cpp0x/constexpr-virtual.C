// PR c++/47067
// { dg-options -std=c++0x }

struct X {
    virtual void x();
    virtual ~X();
};

struct Y {
    virtual void y();
    virtual ~Y();
};

struct Z: X, Y {} z;

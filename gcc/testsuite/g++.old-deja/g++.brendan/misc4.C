// { dg-do assemble  }
// GROUPS passed miscellaneous
// This should not emit an error about A::~A() being redefined; we
// should check that it is a pure virtual.
class A {
public:
    virtual ~A() = 0;
};

A::~A() {}

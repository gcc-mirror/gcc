
// Test to make sure we do not ICE on this invalid program.

struct A
{
    template<int> void foo();
    void bar() { this.A::foo<0>(); }	// { dg-error "" }
};

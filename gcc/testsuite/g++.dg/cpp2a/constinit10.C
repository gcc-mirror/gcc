// PR c++/91360 - Implement C++20 P1143R2: constinit
// { dg-do compile { target c++20 } }
// From PR83428.

struct S1
{
    constexpr S1 ();
    int m_i;
};

struct alignas(64) S2
{
    constexpr S2 ()
    : m_tabS1()
    {}

    S1 m_tabS1[7];
};

constinit S2 objX; // { dg-error ".constinit. variable .objX. does not have a constant initializer" }
// { dg-error "used before its definition" "" { target *-*-* } .-1 }
// // { dg-message "in .constexpr. expansion of" "" { target *-*-* } .-2 }

constexpr S1::S1 ()
: m_i(14)
{}

// PR c++/12316
// Origin: Volker Reichelt <reichelt@igpm.rwth-aachen.de>

// { dg-do compile }
// { dg-options "--param ggc-min-expand=0 --param ggc-min-heapsize=0" }

inline void FOO() {}

template<typename> struct A
{
    A() {}
    ~A() throw() {}
};

template<typename> struct B
{
    static void foo();
    static void bar() { foo(); }
};

struct C {};

template<typename> struct D : C
{
    D() {}
    ~D() { B<void>::bar(); }
};

template<typename> struct E : D<void>
{
    static void baz() {}
    E(A<void>) { baz(); }
};

void BAR()
{
    new E<void>(A<void>());
}

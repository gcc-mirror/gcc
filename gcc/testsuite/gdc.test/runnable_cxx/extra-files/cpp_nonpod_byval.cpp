extern "C" int printf(const char *, ...);

#define Foo(T) \
    T fooD(T param); \
    T fooCpp(T param) \
    { \
        printf("fooCpp %d [%p]\n", param.a, &param); \
        return fooD(T{2 * param.a}); \
    }

struct POD
{
    int a;
};
Foo(POD);

struct CtorOnly
{
    int a;
    CtorOnly() : a(0) {}
    CtorOnly(int a) : a(a) {}
};
Foo(CtorOnly);

struct DtorOnly
{
    int a;
    ~DtorOnly(); // implemented in D
};
Foo(DtorOnly);

struct CtorDtor
{
    int a;
    CtorDtor(int a) : a(a) {}
    ~CtorDtor(); // implemented in D
};
Foo(CtorDtor);

struct Copy
{
    int a;
    Copy(int a) : a(a) {}
    ~Copy(); // implemented in D
    Copy(const Copy &rhs) : a(rhs.a) { printf("cppcpy %d [%p]\n", a, this); }
};
Foo(Copy);

struct CopyAndMove
{
    int a;
    CopyAndMove(int a) : a(a) {}
    ~CopyAndMove(); // implemented in D
    CopyAndMove(const CopyAndMove &rhs) : a(rhs.a) { printf("cppcpy %d [%p]\n", a, this); }
    CopyAndMove(CopyAndMove &&) = default;
};
Foo(CopyAndMove);

struct MoveOnly
{
    int a;
    MoveOnly(int a) : a(a) {}
    ~MoveOnly(); // implemented in D
    MoveOnly(const MoveOnly &) = delete;
    MoveOnly(MoveOnly &&) = default;
};
Foo(MoveOnly);

struct MemberWithCtor
{
    int a;
    CtorOnly m;
};
Foo(MemberWithCtor);

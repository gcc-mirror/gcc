

/**************************************/
// https://issues.dlang.org/show_bug.cgi?id=6538

template allSatisfy(alias F, T...) { enum bool allSatisfy = true; }
template isIntegral(T) { enum bool isIntegral = true; }

void foo(I...)(I sizes)
if (allSatisfy!(isIntegral, sizes)) {}

void test6538a()
{
    foo(42, 86);
}

void bar(T1, T2)(T1 t1, T2 t2)
if (allSatisfy!(isIntegral, t1, t2)) {}

void test6538b()
{
    bar(42, 86);
}

/**************************************/
// https://issues.dlang.org/show_bug.cgi?id=9361

template Sym(alias A)
{
    enum Sym = true;
}

struct S
{
    void foo()() if (Sym!(this)) {}
    void bar()() { static assert(Sym!(this)); }   // OK
}
void test9361a()
{
    S s;
    s.foo();    // fail
    s.bar();    // OK
}

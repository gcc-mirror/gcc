// { dg-do compile }
// { dg-options "-Wcast-result" }

extern(C++) class CPPClass
{
    int a;
}

extern(D) class DClass
{
    int a;
}

void test()
{
    auto cpptod = cast(DClass)new CPPClass; // { dg-warning "cast to 'Wcastresult1.DClass' will produce null result" }
    auto dtocpp = cast(CPPClass)new DClass; // { dg-warning "cast to 'Wcastresult1.CPPClass' will produce null result" }
}

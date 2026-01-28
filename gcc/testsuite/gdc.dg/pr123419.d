// { dg-do compile }
struct S123419 {}

void t123419(T)(T _) {}

void f123419()
{
    t123419(__traits(initSymbol, S123419));
}

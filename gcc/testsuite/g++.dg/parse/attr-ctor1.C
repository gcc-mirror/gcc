// PR c++/6992
// Origin: <petr@scssoft.com>
// { dg-do compile }

// Requires section attribute support

class A
{
    __attribute__((section("whatever"))) A(); // { dg-bogus "" "" { xfail hppa*-*-hpux* } }
};

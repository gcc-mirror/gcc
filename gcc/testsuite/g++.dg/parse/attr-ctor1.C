// PR c++/6992
// Origin: <petr@scssoft.com>
// { dg-do compile }

class A
{
    __attribute__((section("whatever"))) A();
};

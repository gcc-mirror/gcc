// PR target/104213
// { dg-do compile }
// { dg-options "-Wuse-after-free" }

class C
{
    virtual ~C();
};

C::~C() {} // { dg-bogus "used after" }

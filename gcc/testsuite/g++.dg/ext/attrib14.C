// PR c++/13170
// The bogus attribute is ignored, but was in TYPE_ATTRIBUTES during
// parsing of the class, causing some variants to have it and some not.

struct __attribute__((bogus)) A
{
    virtual ~A();
    void foo(const A&);
    void bar(const A&);
};				// { dg-warning "ignored" "" }

void A::foo(const A&)   {}
void A::bar(const A& a) { foo(a); }

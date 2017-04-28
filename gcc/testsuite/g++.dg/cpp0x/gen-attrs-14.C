// { dg-do compile { target c++11 } }
// PR c++/13170
// The bogus attribute is ignored, but was in TYPE_ATTRIBUTES during
// parsing of the class, causing some variants to have it and some not.

struct [[gnu::bogus]] A	// { dg-warning "ignored" }
{
    virtual ~A();
    void foo(const A&);
    void bar(const A&);
};

void A::foo(const A&)   {}
void A::bar(const A& a) { foo(a); }

// PR c++/87729
// { dg-additional-options -Wall }

class Foo
{
public:
    virtual void f(int);	// { dg-warning "hidden" }
};

class Bar : public Foo
{
public:
    virtual void f(short);	// { dg-message "by" }
};

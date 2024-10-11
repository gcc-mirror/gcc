// PR c++/87729
// { dg-additional-options -Wall }

class Foo
{
public:
    virtual void f(int);	// { dg-warning "hidden" }
    void g(int);		// Not virtual, so no warning
};

class Bar : public Foo
{
public:
    virtual void f(short);	// { dg-message "by" }
    virtual void g(short);
};

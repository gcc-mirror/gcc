// { dg-do assemble  }
// { dg-options "-fexceptions" }

class Base {
public:
    virtual ~Base() throw();
};
 
Base::~Base() throw()
{
}
 
class Foo : public Base {
public:
    virtual ~Foo() throw();
};
 
Foo::~Foo() throw()
{
}

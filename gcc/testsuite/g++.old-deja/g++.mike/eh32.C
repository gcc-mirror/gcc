// Build don't link:
// Special g++ Options: -fexceptions

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

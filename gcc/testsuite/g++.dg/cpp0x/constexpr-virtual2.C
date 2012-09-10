// PR c++/54341
// { dg-do compile { target c++11 } }

template<typename T>
struct enable_shared_from_this
{
  constexpr enable_shared_from_this(); // { dg-warning "used but never defined" }

private:
  int mem;
};

class VTableClass {
public:
    virtual void someVirtualMethod() { }
};

class SomeClass : public enable_shared_from_this< SomeClass >, public
VTableClass { };

SomeClass* createInstance()
{
    return new SomeClass;
}

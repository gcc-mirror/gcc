// avoid declaration in cpp file so the C/C++ compiler does no assume
// these are inaccessible from elsewhere
class Cpp15589Base
{
public:
    ~Cpp15589Base();

    virtual void nonVirtual() {}
    int a;
};

class Cpp15589Derived : public Cpp15589Base
{
public:
    Cpp15589Derived();
    ~Cpp15589Derived();
    int b;
};

class Cpp15589BaseVirtual
{
public:
    virtual void beforeDtor() {}

    Cpp15589BaseVirtual();
    virtual ~Cpp15589BaseVirtual();

    virtual void afterDtor() {}
    int c;
};

class Cpp15589DerivedVirtual : public Cpp15589BaseVirtual
{
public:
    Cpp15589DerivedVirtual(); // explicit C++ ctor needed, see https://issues.dlang.org/show_bug.cgi?id=18966
    virtual ~Cpp15589DerivedVirtual();

    virtual void afterDtor() {}

    int d;
};

class Cpp15589IntroducingVirtual : public Cpp15589Base
{
public:
    Cpp15589IntroducingVirtual();
    virtual void beforeIntroducedVirtual() {}
    virtual ~Cpp15589IntroducingVirtual();
    virtual void afterIntroducedVirtual(int) {}

    int e;
};

struct Cpp15589Struct
{
    ~Cpp15589Struct();
    int s;
};

class Base18966
{
public:
    Base18966();
    virtual ~Base18966();
    virtual void vf();
    int x;
};

class A18966
{
public:
    char calledOverloads[8];
    int i;
    A18966();
    virtual void foo();
};

class B18966 : public A18966
{
public:
    B18966();
    void foo() /*override*/;
};

// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::is_explicit.

#include <meta>

class Test {
public:
    explicit Test(int) 
    {
        int a;
        static_assert (std::meta::is_explicit (std::meta::parent_of (^^a)));
    }

    Test(double) 
    {
        int a;
        static_assert (!std::meta::is_explicit (std::meta::parent_of (^^a)));
    }

    explicit operator bool();

    operator int();

    template<typename T>
    explicit operator double();
    
    void member_function();

    virtual void virtual_function();

    template <typename T>
    void template_function(T param);
};

void function();

static_assert (std::meta::is_explicit (^^Test::Test)); // { dg-error "cannot take the reflection of an overload set" }
static_assert (!std::meta::is_explicit (^^Test::~Test));

static_assert (std::meta::is_explicit (^^Test::operator bool));
static_assert (!std::meta::is_explicit (^^Test::operator int));
static_assert (!std::meta::is_explicit (^^Test::operator double));

static_assert (!std::meta::is_explicit (^^Test::member_function));
static_assert (!std::meta::is_explicit (^^Test::virtual_function));
static_assert (!std::meta::is_explicit (^^Test::template_function));
static_assert (!std::meta::is_explicit (^^Test::template_function<int>));

static_assert (!std::meta::is_explicit (^^function));
static_assert (!std::meta::is_explicit (^^Test));
static_assert (!std::meta::is_explicit (^^::));

class Base {
public:
    explicit operator int();
};

class Derived : public Base {
public:
    explicit operator double();

    operator bool();
};

static_assert (std::meta::is_explicit (^^Derived::operator int));
static_assert (std::meta::is_explicit (^^Derived::operator double));
static_assert (!std::meta::is_explicit (^^Derived::operator bool));

class EmptyClass {};
static_assert (!std::meta::is_explicit (^^EmptyClass::EmptyClass)); // { dg-error "cannot take the reflection of an overload set" }


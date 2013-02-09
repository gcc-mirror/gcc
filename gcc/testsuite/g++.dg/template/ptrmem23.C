// PR c++/56247

struct Base {
    void method() {}
};

typedef void (Base::*MemPtr)();

// Template with a member function pointer "non-type parameter".
template<MemPtr func>
struct Wrapper {};

template<class C>
struct Child : public Base {
    // Templated derived class instantiates the Wrapper with the same parameter
    // in two different virtual methods.
    void foo() { typedef Wrapper<&Base::method> W; }
    void bar() { typedef Wrapper<&Base::method> W; }
};

// Instantiate Child with some type.
template class Child<int>;

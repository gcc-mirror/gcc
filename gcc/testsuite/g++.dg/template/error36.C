// PR c++/37719.C

template <typename T>
class foo {
    void bar() throw(int); // { dg-error "throw \\(int\\)" }
};

template <>
void foo<int>::bar() throw(float) {} // { dg-error "throw \\(float\\)" }

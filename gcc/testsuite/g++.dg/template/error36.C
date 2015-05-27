// PR c++/37719.C

template <typename T>
class foo {
    void bar() throw(int); // { dg-message "throw \\(int\\)" }
};

template <>
void foo<int>::bar() throw(float) {} // { dg-error "throw \\(float\\)" }

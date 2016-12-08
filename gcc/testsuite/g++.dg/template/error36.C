// PR c++/37719.C
// { dg-do compile { target c++14_down } }

template <typename T>
class foo {
    void bar() throw(int); // { dg-message "throw \\(int\\)" }
};			   // { dg-warning "deprecated" "" { target c++11 } .-1 }

template <>
void foo<int>::bar() throw(float) {} // { dg-error "throw \\(float\\)" }
				     // { dg-warning "deprecated" "" { target c++11 } .-1 }

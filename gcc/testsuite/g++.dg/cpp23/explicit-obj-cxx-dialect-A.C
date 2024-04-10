// P0847R7
// { dg-do compile { target c++23 } }

struct S {
    void f(this S); // { dg-bogus {explicit object member function only available with '-std=c\+\+23' or '-std=gnu\+\+23'} }
};


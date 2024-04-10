// P0847R7
// { dg-do compile { target c++20_down } }

struct S {
    void f(this S); // { dg-error {explicit object member function only available with '-std=c\+\+23' or '-std=gnu\+\+23'} }
};


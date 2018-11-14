// Origin PR c++/51477
// { dg-do compile { target c++11 } }

struct A
{
    typedef int int T; // { dg-error "two or more data types in declaration" }
    struct T x[1] = { 0 }; // { dg-error "14:field .x. has incomplete type" }
// { dg-message "forward declaration" "" { target c++11 } .-1 }  
};

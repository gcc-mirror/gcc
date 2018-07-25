// { dg-do compile { target c++11 } }

template<typename T> struct S1
{
    enum E : int;   // { dg-message "previous definition" }
    enum E : T;     // { dg-error "different underlying type" }
};
template struct S1<int>; //ok

template<typename T> struct S2
{
    enum E : int;   // { dg-message "previous definition" }
    enum E : T;     // { dg-error "different underlying type" }
};
template struct S2<short>;

template<typename T> struct S3
{
    enum E : int;   // { dg-message "previous definition" }
    enum E : short; // { dg-error "different underlying type" }
};


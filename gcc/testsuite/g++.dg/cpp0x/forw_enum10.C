// { dg-do compile { target c++11 } }

//This error is diagnosed at instantiation time
template<typename T> struct S1
{
    enum E : T;   // { dg-message "previous definition" }
    enum E : int;     // { dg-error "different underlying type" }
};
template struct S1<short>; // { dg-message "required from here" }

template<typename T> struct S2
{
    enum E : T;
    enum E : T;
};
template struct S2<short>;

template<typename T1, typename T2> struct S3
{
    enum E : T1;
    enum E : T2;
};
template struct S3<short,short>;

template<typename T1, typename T2> struct S4
{
    enum E : T1; // { dg-message "previous definition" }
    enum E : T2; // { dg-error "different underlying type" }
};
template struct S4<short,char>; // { dg-message "required from here" }

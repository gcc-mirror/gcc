// { dg-do compile }
// { dg-options "-std=c++0x" }

//This instatiation is ok
template<typename T> struct S1
{
    enum E : int;
    enum E : T;
};
template struct S1<int>; //ok

//This error is diagnosed at instantiation time
template<typename T> struct S2
{
    enum E : int;   // { dg-error "previous definition" }
    enum E : T;     // { dg-error "different underlying type" }
};
template struct S2<short>; // { dg-message "instantiated from here" }

//This error is diagnosed at compilation time
template<typename T> struct S3
{
    enum E : int;   // { dg-error "previous definition" }
    enum E : short; // { dg-error "different underlying type" }
};


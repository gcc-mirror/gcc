// { dg-do assemble  }

template <class T>
struct S1; // { dg-error "" } previous declaration

template <class T, class U>
struct S1 {}; // { dg-error "" } used 1 template parameter

template <class T = int> // { dg-error "" } original def of default
struct S2; 

template <class T = int>
struct S2; // { dg-error "" } redefinition of default

template <class T> // { dg-error "" } template parameter
struct S3;

template <int I>
struct S3; // { dg-error "" } redeclared here

template <template <class T> class C>
struct S3; // { dg-error "" } redeclared here

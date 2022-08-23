// { dg-do assemble  }

template <class T>
struct S1; // { dg-message "previous declaration" } 

template <class T, class U>
struct S1 {}; // { dg-error "redeclared" } used 1 template parameter

template <class T = int> // { dg-message "original definition" }
struct S2; 

template <class T = int> // { dg-error "redefinition of default" } 
struct S2;

template <class T> // { dg-error "template parameter" } 
struct S3;

template <int I> // { dg-message "note: redeclared here" } 
struct S3;

template <template <class T> class C> // { dg-message "note: redeclared here" } 
struct S3;

// Build don't link:

template <class T>
struct S1; // ERROR - previous declaration

template <class T, class U>
struct S1 {}; // ERROR - used 1 template parameter

template <class T = int> // ERROR - original def of default
struct S2; 

template <class T = int>
struct S2; // ERROR - redefinition of default

template <class T> // ERROR - template parameter
struct S3;

template <int I>
struct S3; // ERROR - redeclared here

template <template <class T> class C>
struct S3; // ERROR - redeclared here

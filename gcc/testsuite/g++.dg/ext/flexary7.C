// PR c++/68613 - initializer-string for array of chars is too long error
// on flexible array member
// { dg-do compile }
// { dg-options "-Wpedantic -Wno-error=pedantic" }

struct FlexChar {
    int n;
    char a[];       // { dg-warning "forbids flexible array member" }
};

struct FlexChar ac =
  { 4, { "abc" } }; // { dg-warning "initialization of a flexible array member" }


#if !__cplusplus
typedef __WCHAR_TYPE__ wchar_t;
#endif

struct FlexWchar {
    int n;
    wchar_t a[];    // { dg-warning "forbids flexible array member" }
};

struct FlexWchar awc =
  { 3, { L"ab" } }; // { dg-warning "initialization of a flexible array member" }


struct FlexInt {
    int n;
    int a[];        // { dg-warning "forbids flexible array member" }
};

// Verify that no warning is issued for the case when a flexible array
// member is not initialized (i.e., that a -Wmissing-field-initializer
// isn't issued) because such arrays need not have any elements.
struct FlexInt ai0 =
  { 0 };

struct FlexInt ai0_ =
  { 0, { } };      // { dg-warning "initialization of a flexible array member" }

struct FlexInt ai2 =
  { 2, { 1, 2 } }; // { dg-warning "initialization of a flexible array member" }


#if __cplusplus

template <class T>
struct FlexT {
    int n;
    T a[];          // { dg-warning "forbids flexible array member" }
};

struct FlexT<char> atc =
  { 4, { "abc" } }; // { dg-warning "initialization of a flexible array member" }

#endif

// PR c++/19244
// { dg-additional-options "-Wno-non-c-typedef-for-linkage" }

typedef struct { void f(); } f;
void f::f() { }

// PR c++/19244

typedef struct { void f(); } f;
void f::f() { }

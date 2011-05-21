// PR c++/48780
// { dg-options "-std=c++0x -fabi-version=0" }

typedef __builtin_va_list __gnuc_va_list;
typedef __gnuc_va_list va_list;

enum struct A : short { X };

void foo(int x, ...) {
   va_list vl;
   __builtin_va_start(vl, x);
   enum A t = __builtin_va_arg(vl, enum A);
   __builtin_va_end(vl);
}

int main() {
   foo(0, A::X);
}

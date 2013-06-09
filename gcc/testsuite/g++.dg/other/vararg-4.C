// PR c++/37404

typedef __builtin_va_list __gnuc_va_list;
typedef __gnuc_va_list va_list;

template <class> struct S { static void foo () { } };
template <class T, int N>
struct S<T [N]> { static void foo () { T(); } };

int main () {
  S<va_list>::foo();
}

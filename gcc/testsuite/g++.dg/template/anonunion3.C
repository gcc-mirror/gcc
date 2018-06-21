// PR c++/86182
// { dg-do compile { target c++11 } }

extern "C" int printf (const char *, ...);

template<typename T> static char const * f(T *t) {
 T u(*t);
 u.x = "hello world";
 printf("%s\n", u.x);
 return "initialized";
}

int main() {
 union { char const *x = f(this); };
 printf("%s\n", x);
}

// PR c++/26068

extern "C" auto int a; // { dg-error "linkage|two or more data types" }
extern "C" register int b; // { dg-error "linkage" }
extern "C" static void c(); // { dg-error "linkage" }
extern "C" extern void d(); // { dg-error "linkage" }
extern "C" mutable int e; // { dg-error "linkage" }

extern "C" {
  static void f();
}

// PR C++/100281
// { dg-do compile }

typedef int & __attribute__((mode (SI))) __ref32_t;

void foo () {
  unsigned int b = 100;
  __ref32_t a = b; /* { dg-error "cannot bind non-const lvalue reference of type '__ref32_t'.*" } */
}

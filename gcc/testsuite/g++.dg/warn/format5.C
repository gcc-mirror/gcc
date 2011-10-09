// PR c++/38980
// { dg-options "-Wformat" }

extern "C"
int printf(const char *format, ...) __attribute__((format(printf, 1, 2) ));

const char fmt1[] = "Hello, %s";

void f()
{
  printf(fmt1, 3); // { dg-warning "expects argument" }
}

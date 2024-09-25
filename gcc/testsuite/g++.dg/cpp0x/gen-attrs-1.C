// { dg-additional-options "-Wno-c++11-extensions" }

int **** [[gnu::format(printf, 1, 2)]] foo(const char *, ...); // { dg-warning "only applies to function types" }

// { dg-do compile { target c++11 } }

int **** [[gnu::format(printf, 1, 2)]] foo(const char *, ...); // { dg-warning "ignored" }

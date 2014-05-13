// { dg-options "-pedantic" }

void (*p)();

void f() {
  (void *)p; // { dg-warning "forbids cast" }
}

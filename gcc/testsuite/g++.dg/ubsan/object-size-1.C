// { dg-do compile }
// { dg-options "-Wpedantic -Wno-error=pedantic -fsanitize=undefined -fpermissive" }

struct T
{
  int c; char d[];   // { dg-warning "forbids flexible array member" }
};

struct T t = { 1, "a" }; // { dg-warning "initialization of a flexible array member " }

int
baz (int i)
{
  return t.d[i];
}

int
main (void)
{
  baz (3);
}

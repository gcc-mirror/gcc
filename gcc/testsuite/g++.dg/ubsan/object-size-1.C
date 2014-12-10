// { dg-do compile }
// { dg-options "-fsanitize=undefined -fpermissive" }

struct T { int c; char d[]; };

struct T t = { 1, "a" }; // { dg-warning "initializer-string for array of chars is too long" }

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

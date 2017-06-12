// { dg-do compile }
// { dg-options "-Wstrict-aliasing=2 -fstrict-aliasing" }

struct foo {
  char c;
  char d;
  short s;
  int i;
} bar;

int
sub1 (long long int foobar)
{
  struct foo *tmp = (struct foo *) &foobar; // { dg-warning "type-punned pointer will" }
  return tmp->i;
}

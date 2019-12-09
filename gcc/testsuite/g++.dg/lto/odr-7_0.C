// { dg-lto-do link }

struct bar // { dg-lto-message "type name 'bar' should match type name 'foobar<float>'" }
{
  int xxx;
};

struct foo // { dg-lto-warning "8: 'struct foo' violates the C\\+\\+ One Definition Rule" }
{
  bar a;
};

foo myfoo2;

int
main()
{
}

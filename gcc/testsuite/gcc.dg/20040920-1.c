/* { dg-do compile } */
int bob;
struct a
{
  int foo;
};
int main(void)
{
  struct a bar;
  bob(5); /* { dg-error "called object 'bob' is not a function" } */
  bar.foo(); /* { dg-error "called object 'bar.foo' is not a function" } */
}

struct Foo {
    int *p;
    struct X {
	int a,b,c,d,e,*f;
    } x;
} *init, *init2;

struct X __attribute__((const)) foo(struct X);
struct Foo __attribute__((const)) foo2(struct Foo);

void bar1 (void)
{
  init->x = foo (init2->x);
}
void bar2 (void)
{
  init->x = foo (init->x);
}
void bar3 (void)
{
  *init = foo2 (*init2);
}

typedef struct _foo foo;
extern foo bar;
struct _foo {
  int a;
};

void baz(void)
{
  bar.a = 0;
}

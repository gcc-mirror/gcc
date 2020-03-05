/* PR middle-end/90840 */
struct S { long long a; int b; };
struct S foo (void);
struct __attribute__((packed)) T { long long a; char b; };
struct T baz (void);

void
bar (void)
{
  _Complex long double c;
  *(struct S *) &c = foo ();
}

void
qux (void)
{
  _Complex long double c;
  *(struct T *) &c = baz ();
}

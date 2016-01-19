void foo (int *);

template <typename T>
void bar (void (T[5])); // { dg-error "array of 'void'" }

void
baz (void)
{
  bar (foo); // { dg-bogus "" }
  bar<void> (0); // { dg-error "no matching function" }
}

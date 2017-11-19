namespace foo {
void baz(int);
}

void bar(int foo) {
  foo::baz (3);
}

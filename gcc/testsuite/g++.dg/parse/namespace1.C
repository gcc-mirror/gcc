namespace foo {
void baz(int);
}

int bar(int foo) {
  foo::baz (3);
}

class Foo;

void
bar(void* p)
{
  static_cast<Foo*>(p)->~Foo(); // { dg-error "" }
}

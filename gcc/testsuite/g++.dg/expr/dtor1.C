class Foo; // { dg-message "" }

void
bar(void* p)
{
  static_cast<Foo*>(p)->~Foo(); // { dg-error "" }
}

class Foo; // { dg-error "" }

void
bar(void* p)
{
  static_cast<Foo*>(p)->~Foo(); // { dg-error "" }
}

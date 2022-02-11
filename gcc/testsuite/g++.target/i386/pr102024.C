// PR target/102024
// { dg-do compile }

struct S { float a; int : 0; float b; };
void foo (struct S x);

void
bar (void)
{
  struct S s = { 0.0f, 0.0f };
  foo (s);	// { dg-bogus "the ABI of passing C structures with zero-width bit-fields has changed in GCC 12.1" }
}

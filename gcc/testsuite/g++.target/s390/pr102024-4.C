// PR target/102024
// { dg-do compile }

/* struct S would not be passed as single value anyway so no warning expected.  */

struct T { float a; int :0; };
struct S { struct T t; int x; };
void foo (struct S x);

void
bar (void)
{
  struct S s = { { 0.0f }, 0 };
  foo (s);	// { dg-bogus "with zero-width bit fields members changed in GCC 12" }
}

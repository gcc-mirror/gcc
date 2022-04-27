// PR target/102024
// { dg-do compile }

/* struct would never be passed in an FPR so no warning expected.  */

struct S { float a; int :0; float b; };
void foo (struct S x);

void
bar (void)
{
  struct S s = { 0.0f };
  foo (s);	// { dg-bogus "with zero-width bit fields members changed in GCC 12" }
}

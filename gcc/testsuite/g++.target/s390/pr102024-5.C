// PR target/102024
// { dg-do compile }

struct U { float a; int :0; };
struct T { struct U u; };
struct S { struct T t; };
void foo (struct S x);

void
bar (void)
{
  struct S s = { { { 0.0f } } };
  foo (s);	// { dg-message "with zero-width bit fields members changed in GCC 12" }
}

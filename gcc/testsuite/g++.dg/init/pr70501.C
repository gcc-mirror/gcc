/* { dg-options "" } Not pedantic */

typedef int v4si __attribute__ ((vector_size (16)));

struct S { v4si v; };

void
fn2 (int i, int j)
{
  struct S s = { .v = i <= j + (v4si){(1, 2)} };
}

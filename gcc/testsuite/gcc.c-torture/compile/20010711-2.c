typedef unsigned long long value;

void foo (value *v) {}

void test ()
{
  value v;
  foo (&v);
  if (v-- == 1)
    foo (&v);
}

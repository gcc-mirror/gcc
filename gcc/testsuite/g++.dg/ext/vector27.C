// PR c++/58845

void foo()
{
  int v __attribute__((vector_size(8)));
  v = v || v;			// { dg-bogus "" "" { xfail *-*-* } }
}

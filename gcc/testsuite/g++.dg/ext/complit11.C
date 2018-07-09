// { dg-do compile }
// { dg-options "" }

struct A { int i; };

template<int t>
void foo()
{
    ((struct A) { 0 }).i += 1;	// { dg-error "lvalue" }
}

void g(void)
{
  foo<0>();
}


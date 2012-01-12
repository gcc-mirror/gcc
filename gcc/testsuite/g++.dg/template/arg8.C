template<typename T, void, typename U> // { dg-error "void" }
void foo(T, U, int) {}

void bar()
{
  foo(0, 0, 0);			// { dg-error "no match" }
}

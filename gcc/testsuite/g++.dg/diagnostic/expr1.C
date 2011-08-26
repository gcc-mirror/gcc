// PR c++/50059

int i;
struct A { };
void f(A);
void g()
{
  f(i = 0);			// { dg-error "i = 0" }
}

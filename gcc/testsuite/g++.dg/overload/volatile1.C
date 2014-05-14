// PR c++/48118

struct A { };

void f (A);			// { dg-message "" }
void (*g)(A);

int main()
{
  volatile A a;
  f(a);				// { dg-error "" }
  g(a);				// { dg-error "" }
}

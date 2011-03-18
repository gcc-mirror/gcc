// PR c++/48118
// { dg-prune-output "note" }

struct A { };

void f (A);			// { dg-error "argument 1" }
void (*g)(A);

int main()
{
  volatile A a;
  f(a);				// { dg-error "no match" }
  g(a);				// { dg-error "no match" }
}

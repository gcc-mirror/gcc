// PR c++/80178
// { dg-do compile { target c++11 } }
// { dg-options "-Wabi=10 -fdump-tree-gimple" }
// { dg-final { scan-tree-dump "foo .&D" "gimple" } }

struct A {
  A();
  A &operator=(A &&o);
  void *p;
};
void notdefined(A);

void foo(A) { }			// { dg-warning "calling convention" }

A baz()				// { dg-warning "calling convention" }
{
  return {};
}

void bar() {
  foo({});			// { dg-warning "calling convention" }
  notdefined({});		// { dg-warning "calling convention" }
  baz();			// { dg-warning "calling convention" }
}

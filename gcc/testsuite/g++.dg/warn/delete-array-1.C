// PR c++/29185

int a [1];
struct S { int a [1]; } s;

void foo (S *p)
{
  delete a;    // { dg-warning "deleting array" }
  delete s.a;  // { dg-warning "deleting array" }
  delete p->a; // { dg-warning "deleting array" }
}

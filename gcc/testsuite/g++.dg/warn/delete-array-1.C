// PR c++/29185

int a [1];
struct S { int a [1]; } s;

void foo (S *p)
{
  delete a;    // { dg-warning "deleting array|-Wfree-nonheap-object" }
  delete s.a;  // { dg-warning "deleting array|-Wfree-nonheap-object" }
  delete p->a; // { dg-warning "deleting array|-Wfree-nonheap-object" }
}

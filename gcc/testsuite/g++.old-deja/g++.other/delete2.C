// { dg-do assemble  }

struct foo {
  operator char*() const;
};

void bar(foo a) {
  delete a; // should be accepted
  delete[] a; // should be accepted
  char b[1];
  delete b; // { dg-warning "deleting array" } expecting pointer type
  // { dg-warning "-Wfree-nonheap-object" "" { target *-*-* } .-1 }
  delete[] b; // { dg-warning "deleting array" } expecting pointer type
  // { dg-warning "-Wfree-nonheap-object" "" { target *-*-* } .-1 }
}

// Build don't link:

struct foo {
  operator char*() const;
};

void bar(foo a) {
  delete a; // should be accepted - XFAIL *-*-*
  delete[] a; // should be accepted - XFAIL *-*-*
  char b[1];
  delete b; // ERROR - expecting pointer type
  delete[] b; // ERROR - expecting pointer type
}

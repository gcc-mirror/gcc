// { dg-do compile { target c++11 } }
// { dg-options "-Wdangling-reference" }

int& ref(const int&);
int&& rref(const int&);

void
g ()
{
  const int& r1 = ref (1); // { dg-warning "dangling reference" }
  int& r2 = ref (2); // { dg-bogus "dangling reference" }
  auto& r3 = ref (3); // { dg-bogus "dangling reference" }
  int&& r4 = rref (4); // { dg-warning "dangling reference" }
  auto&& r5 = rref (5); // { dg-warning "dangling reference" }
  const int&& r6 = rref (6); // { dg-warning "dangling reference" }
}

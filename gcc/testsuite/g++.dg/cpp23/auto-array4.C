// PR c++/102414
// PR c++/101874
// { dg-do compile { target c++11 } }
// { dg-options "" }

void
f (int i)
{
  auto x[i] = { 0 }; // { dg-message "variable-length array of .auto." }
  auto(*p)[i] = (int(*)[i])0; // { dg-message "variable-length array of .auto." }
  int a[3];
  auto (*a1)[0/0] = &a; // { dg-message "variable-length array of .auto." }
// { dg-warning "division by zero" "" { target *-*-* } .-1 }
}

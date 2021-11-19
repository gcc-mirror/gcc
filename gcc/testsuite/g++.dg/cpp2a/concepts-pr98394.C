// PR c++/98394
// { dg-do compile { target c++20 } }

template<int...>
concept C = true;

template<int, int>
concept D = true;

int main() {
  if (C<1>); // { dg-bogus "does not constrain a type" }
  if (D<1>); // { dg-error "wrong number of template arguments" }
	     // { dg-bogus "does not constrain a type" "" { target *-*-* } .-1 }
}

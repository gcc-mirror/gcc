// Verify we issue -Wparentheses warnings at template definition time
// (for suitable non-dependent expressions).
// { dg-additional-options "-Wparentheses" }

struct X { operator bool(); };
struct Y { Y& operator=(const Y&); operator bool(); };
struct Z { int m; operator bool(); };

template<class T>
void f() {
  int n, m;
  if (n = m) { } // { dg-warning "parentheses" }

  X x1, x2;
  if (x1 = x2) { } // { dg-warning "parentheses" }

  Y y1, y2;
  if (y1 = y2) { } // { dg-warning "parentheses" }

  Z z1, z2;
  if (z1 = z2) { } // { dg-warning "parentheses" }

  bool b;
  b = m = n; // { dg-warning "parentheses" }
  b = x1 = x2; // { dg-warning "parentheses" }
  b = y1 = y2; // { dg-warning "parentheses" }
  b = z1 = z2; // { dg-warning "parentheses" }
}

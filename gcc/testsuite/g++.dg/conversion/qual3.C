// PR c++/88128 - DR 330: Qual convs and pointers to arrays of pointers.
// { dg-do compile { target c++17 } }

using P = int *(*)[3];
using Q = const int *const (*)[3];
using Qi = const int *[3];
using Q2 = Qi const *;
using R = const int *const (*)[4];
using S = const int *const (*)[];
using T = const int *(*)[];

void
f (P p, Q q, Q2 q2, R r, S s, T t)
{
  q = p;
  q2 = p;
  r = p; // { dg-error "cannot convert" }
  t = p; // { dg-error "cannot convert" }
  s = t;
  t = s; // { dg-error "invalid conversion" }

  // Test const_cast.
  const_cast<P>(q);
  const_cast<P>(q2);
  const_cast<Q>(p);
  const_cast<Q2>(p);
  const_cast<S>(p); // { dg-error "3:invalid .const_cast." }
  const_cast<P>(s); // { dg-error "3:invalid .const_cast." }
  const_cast<S>(q); // { dg-error "3:invalid .const_cast." }
  const_cast<S>(q2); // { dg-error "3:invalid .const_cast." }
  const_cast<Q>(s); // { dg-error "3:invalid .const_cast." }
  const_cast<Q2>(s); // { dg-error "3:invalid .const_cast." }
  const_cast<T>(s);
  const_cast<S>(t);
  const_cast<T>(q); // { dg-error "3:invalid .const_cast." }
  const_cast<Q>(t); // { dg-error "3:invalid .const_cast." }

  // Test reinterpret_cast.
  reinterpret_cast<P>(q); // { dg-error "3:.reinterpret_cast. \[^\n\r]* casts away qualifiers" }
  reinterpret_cast<P>(q2); // { dg-error "3:.reinterpret_cast. \[^\n\r]* casts away qualifiers" }
  reinterpret_cast<Q>(p);
  reinterpret_cast<Q2>(p);
  reinterpret_cast<S>(p);
  reinterpret_cast<P>(s); // { dg-error "3:.reinterpret_cast. \[^\n\r]* casts away qualifiers" }
  reinterpret_cast<S>(q);
  reinterpret_cast<S>(q2);
  reinterpret_cast<Q>(s);
  reinterpret_cast<Q2>(s);
  reinterpret_cast<T>(s); // { dg-error "3:.reinterpret_cast. \[^\n\r]* casts away qualifiers" }
  reinterpret_cast<S>(t);
  reinterpret_cast<T>(q); // { dg-error "3:.reinterpret_cast. \[^\n\r]* casts away qualifiers" }
  reinterpret_cast<Q>(t);
}

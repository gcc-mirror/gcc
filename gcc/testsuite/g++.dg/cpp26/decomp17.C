// P1061R10 - Structured Bindings can introduce a Pack
// { dg-do compile { target c++11 } }
// { dg-options "" }

typedef int V __attribute__((vector_size (16 * sizeof (int))));

template <int N>
void
foo ()
{
  V v = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16 };
  auto [va, vb, vc, vd, ...ve, vf, vg, vh, vi, vj, vk, vl, vm, vn, vo, vp, vq, vr] = v;
					// { dg-warning "structured binding packs only available with" "" { target { c++17 && c++23_down } } .-1 }
					// { dg-warning "structured bindings only available with" "" { target c++14_down } .-2 }
					// { dg-error "18 names provided for structured binding" "" { target *-*-* } .-3 }
					// { dg-message "while '__vector\\\(16\\\) int' decomposes into 16 elements" "" { target *-*-* } .-4 }
  _Complex double c = 1.0 + 2.0i;
  auto [...ca, cb, cc, cd] = c;		// { dg-warning "structured binding packs only available with" "" { target { c++17 && c++23_down } } }
					// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1 }
					// { dg-error "4 names provided for structured binding" "" { target *-*-* } .-2 }
					// { dg-message "while '__complex__ double' decomposes into 2 elements" "" { target *-*-* } .-3 }
}

int
main ()
{
  foo <0> ();
}

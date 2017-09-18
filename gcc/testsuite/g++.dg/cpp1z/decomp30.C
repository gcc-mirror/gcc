// PR c++/81258
// { dg-options -std=c++17 }

int a[2];
auto [b, c] (a);
auto [d, e] { a };
auto [f, g] = a;
auto [h, i] ( a, a );	// { dg-error "invalid initializer for structured binding declaration" }
auto [j, k] { a, a };	// { dg-error "invalid initializer for structured binding declaration" }
auto [l, m] = { a };	// { dg-error "deducing from brace-enclosed initializer list requires" }
auto [n, o] {};		// { dg-error "invalid initializer for structured binding declaration" }
auto [p, q] ();		// { dg-error "invalid initializer for structured binding declaration" }

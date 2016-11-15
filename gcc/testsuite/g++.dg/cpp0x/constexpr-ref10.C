// { dg-do compile { target c++11 } }

int &&r = 42;
static_assert (r, "");		// { dg-error "temporary" }
// { dg-prune-output "assert" }

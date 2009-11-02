// PR c++/41774
// { dg-do compile }

namespace std __attribute__ ((__visibility__ ("default"))) {
#pragma GCC visibility pop	// { dg-warning "no matching push for" }
}

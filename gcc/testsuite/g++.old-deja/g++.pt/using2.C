// Build don't link:
// 
// Copyright (C) 2001 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 22 May 2001 <nathan@codesourcery.com>

// Bug 2184. Using decls in templates weren't doing the right thing.

namespace N {
  template <class T>
  class vector {};
}

void g(const int&) {
  using N::vector;
  typedef vector<int> V;
}

template <class J>
void f(const J&) {
  using N::vector;
  typedef vector<int> V;
}

int main() {
  f(0);
}

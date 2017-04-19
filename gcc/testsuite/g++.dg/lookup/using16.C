// Copyright (C) 2007 Free Software Foundation
// Contributed by Ollie Wild <aaw@google.com>
// { dg-do compile }

namespace M {
  struct S {}; // { dg-message "struct M::S" "candidate 1" }
}

namespace N {
  int S;
  struct S {}; // { dg-message "struct N::S" "candidate 2" }
}

using namespace M;
using namespace N;

struct ::S s; // { dg-bogus "ambiguous.*ambiguous" "duplicate error" }
// { dg-error "reference to 'S' is ambiguous|invalid type" "" { target *-*-* } .-1 }}

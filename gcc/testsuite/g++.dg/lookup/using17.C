// Copyright (C) 2007 Free Software Foundation
// Contributed by Ollie Wild <aaw@google.com>
// { dg-do compile }

namespace M {
  struct S {}; // { dg-error "struct M::S" "candidate 2" }
}

int S;
struct S {}; // { dg-error "candidates are: struct S" "candidate 1" }

using namespace M;

struct S s; // { dg-error "reference to 'S' is ambiguous|invalid type in declaration" "" }

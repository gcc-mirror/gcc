// Copyright (C) 2007 Free Software Foundation
// Contributed by Ollie Wild <aaw@google.com>
// { dg-do compile }

namespace M {
  struct S {}; // { dg-message "candidates are: .struct M::S." "candidate 1" }
}

int S;
struct S {}; // { dg-message ".struct S." "candidate 2" }

using namespace M;

struct S s; // { dg-error "reference to 'S' is ambiguous|invalid type in declaration" }

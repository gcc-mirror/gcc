// Copyright (C) 2007 Free Software Foundation
// Contributed by Ollie Wild <aaw@google.com>
// { dg-do compile }

namespace M {
  struct S {}; // { dg-message "candidate 1: 'struct M::S'" }
}

int S;
struct S {}; // { dg-message "candidate 2: 'struct S'" }

using namespace M;

struct S s; // { dg-error "reference to 'S' is ambiguous|invalid type in declaration" }

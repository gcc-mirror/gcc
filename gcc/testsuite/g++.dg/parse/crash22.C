// Copyright (C) 2005 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 4 Jan 2005 <nathan@codesourcery.com>

// PR 19030: ICE
// Origin: Volker Reichelt <reichelt@gcc.gnu.org>

struct A;

namespace N
{
  struct A;
}

using namespace N;

int A::i; // { dg-error "not been declared|declared here" "" }
int A::i; // { dg-error "not been declared|redefinition of" "" }

namespace N
{
    struct C;
    struct C {};
}

class D : N::C {};

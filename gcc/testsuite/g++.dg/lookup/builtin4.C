// Copyright (C) 2007 Free Software Foundation
// Contributed by Ollie Wild <aaw@google.com>
// Origin: Volker Reichelt <reichelt@gcc.gnu.org>
// { dg-do compile }

// PR 31749: ICE with redeclaration of builtin

namespace std
{
  union abort;
}

union abort;

using std::abort; // { dg-error "" }

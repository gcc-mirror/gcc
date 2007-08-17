// Copyright (C) 2007 Free Software Foundation
// Contributed by Ollie Wild <aaw@google.com>
// { dg-do compile }


// C++ Standard, 7.3.3, clause 10:
// "Since a using-declaration is a declaration, the restrictions on
// declarations of the same name in the same declarative region (3.3) also
// apply to using-declarations."

namespace M
{
  union A;
  void B();
}

void A();
union B;

using M::A;
using M::B;

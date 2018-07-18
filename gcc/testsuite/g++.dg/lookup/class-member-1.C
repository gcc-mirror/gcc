// Copyright (C) 2002 Free Software Foundation
// Origin: PR/7621, Vaclav.Haisman@logout.sh.cvut.cz
// Contributed by Gabriel Dos Reis <gdr@integrable-solutions.net>

struct A { };

int main()
{
  A a;
  a.i = 9;           // { dg-error "no member" }
}

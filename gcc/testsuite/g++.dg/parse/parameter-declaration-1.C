// Contributed by Gabriel Dos Reis <gdr@integrable-solutions.net>
// Origin: Robert Schiele; PR C++/8799
// { dg-do compile }

struct {
  a(void = 0; a(0), a(0) // { dg-error "" }
// { dg-error "-:expected" "" { target *-*-* } .+1 }

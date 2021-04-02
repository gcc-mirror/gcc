// Contributed by Gabriel Dos Reis <gdr@integrable-solutions.net>
// Origin: Robert Schiele; PR C++/8799
// { dg-do compile }

struct {
  a(void = 0; a(0), a(0) // { dg-error "invalid|cannot|before|forbid|member" }
// { dg-error "25:end of input" "" { target *-*-* } .-1 }

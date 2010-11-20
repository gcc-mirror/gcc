// Contributed by Gabriel Dos Reis <gdr@integrable-solutions.net>
// Origin: Robert Schiele; PR C++/8799
// { dg-do compile }

struct {			// { dg-error "" }
   a(void = 0; a(0), a(0)	// { dg-error "" "" { target *-*-* } }

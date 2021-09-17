/* { dg-do compile } */

int x;
double d, g;

void
foo (int y, double e, long double f)
{
  double v;
  int r, r2 = 0;
  #pragma omp atomic capture compare
  v = if (d == e) { d = f; };	/* { dg-error "expected expression" } */
  #pragma omp atomic compare
  if;				/* { dg-error "expected '\\\(' before ';' token" } */
  #pragma omp atomic compare
  if (d >= e) { d = e; }	/* { dg-error "expected '==', '<' or '>' comparison in 'if' condition" } */
  #pragma omp atomic compare
  if (d <= e) { d = e; }	/* { dg-error "expected '==', '<' or '>' comparison in 'if' condition" } */
  #pragma omp atomic compare
  if (d != e) { d = e; }	/* { dg-error "expected '==', '<' or '>' comparison in 'if' condition" } */
  #pragma omp atomic compare
  if (d + e) { d = e; }		/* { dg-error "expected '==', '<' or '>' comparison in 'if' condition" } */
  #pragma omp atomic capture compare
  { r = d >= e; if (r) { d = f; } }	/* { dg-error "expected '==', '<' or '>' comparison in 'if' condition" "" { target c } } */
  #pragma omp atomic capture compare	/* { dg-error "invalid form of '#pragma omp atomic' before 'd'" "" { target c++ } .-1 } */
  { r = d <= e; if (r) { d = f; } }	/* { dg-error "expected '==', '<' or '>' comparison in 'if' condition" "" { target c } } */
  #pragma omp atomic capture compare	/* { dg-error "invalid form of '#pragma omp atomic' before 'd'" "" { target c++ } .-1 } */
  { r = d > e; if (r) { d = f; } }	/* { dg-error "expected '==', '<' or '>' comparison in 'if' condition" "" { target c } } */
  #pragma omp atomic capture compare	/* { dg-error "invalid form of '#pragma omp atomic' before 'd'" "" { target c++ } .-1 } */
  { r = d < e; if (r) { d = f; } }	/* { dg-error "expected '==', '<' or '>' comparison in 'if' condition" "" { target c } } */
  #pragma omp atomic capture compare	/* { dg-error "invalid form of '#pragma omp atomic' before 'd'" "" { target c++ } .-1 } */
  { r = d != e; if (r) { d = f; } }	/* { dg-error "expected '==', '<' or '>' comparison in 'if' condition" "" { target c } } */
  #pragma omp atomic capture compare	/* { dg-error "invalid form of '#pragma omp atomic' before 'd'" "" { target c++ } .-1 } */
  { r = d + e; if (r) { d = f; } }	/* { dg-error "expected '==', '<' or '>' comparison in 'if' condition" "" { target c } } */
  #pragma omp atomic capture compare	/* { dg-error "invalid form of '#pragma omp atomic' before 'd'" "" { target c++ } .-1 } */
  { r = d == e; if (r2) { d = f; } }	/* { dg-error "invalid form of '#pragma omp atomic compare' before '\{' token" } */
  #pragma omp atomic capture compare
  if (d > e) { d = e; }			/* { dg-error "expected '==' comparison in 'if' condition" } */
  #pragma omp atomic capture compare
  if (d < e) { d = e; }			/* { dg-error "expected '==' comparison in 'if' condition" } */
  #pragma omp atomic compare
  if (d < e) d = e;			/* { dg-error "expected '\{' before 'd'" } */
  #pragma omp atomic compare
  if (d == e) d = e + 1.0;		/* { dg-error "expected '\{' before 'd'" } */
  #pragma omp atomic compare
  if (d < e) { d += e; }		/* { dg-error "expected '=' before '\\\+=' token" } */
  #pragma omp atomic compare
  if (d < e) { d = e };			/* { dg-error "expected ';' before '\}' token" } */
  #pragma omp atomic compare
  if (d < e) { d = e; e = 1.0; }	/* { dg-error "expected '\}' before 'e'" } */
  #pragma omp atomic compare
  if (e == d) { d = f; };		/* { dg-error "invalid form of '#pragma omp atomic compare' before ';' token" } */
  #pragma omp atomic compare
  if (e == d) { g = f; };		/* { dg-error "invalid form of '#pragma omp atomic compare' before ';' token" } */
  #pragma omp atomic compare
  if (d < e) { g = e; };		/* { dg-error "invalid form of '#pragma omp atomic compare' before ';' token" } */
  #pragma omp atomic compare
  if (d > e) { g = e; };		/* { dg-error "invalid form of '#pragma omp atomic compare' before ';' token" } */
  #pragma omp atomic compare
  if (d < e) { d = g; };		/* { dg-error "invalid form of '#pragma omp atomic compare' before ';' token" } */
  #pragma omp atomic compare
  if (d > e) { d = g; };		/* { dg-error "invalid form of '#pragma omp atomic compare' before ';' token" } */
  #pragma omp atomic compare
  if (d == e) { d = f; } else ;		/* { dg-error "unexpected 'else'" } */
  #pragma omp atomic compare capture
  { if (d == e) { d = f; } else { v = d; } v = d; }		/* { dg-error "unexpected 'else'" } */
  #pragma omp atomic compare
  if (d < e) { d = e; } else { v = d; }	/* { dg-error "unexpected 'else'" } */
  #pragma omp atomic compare capture
  if (d == e) { d = f; } else v = d;	/* { dg-error "expected '\{' before 'v'" } */
  #pragma omp atomic compare capture
  if (d == e) { d = f; } else { v += d;	}	/* { dg-error "expected '=' before '\\\+=' token" } */
  #pragma omp atomic compare capture
  if (d == e) { d = f; } else { v = e; }	/* { dg-error "invalid form of '#pragma omp atomic compare' before ';' token" } */
  #pragma omp atomic compare capture
  if (d == e) { d = f; } else { v = d };	/* { dg-error "expected ';' before '\}' token" } */
  #pragma omp atomic compare capture
  if (d == e) { d = f; };		/* { dg-error "expected 'else' before ';' token" } */
  #pragma omp atomic compare
  x++;					/* { dg-error "invalid form of 'pragma omp atomic compare'" } */
  #pragma omp atomic compare
  x--;					/* { dg-error "invalid form of 'pragma omp atomic compare'" } */
  #pragma omp atomic compare
  ++x;					/* { dg-error "invalid form of 'pragma omp atomic compare'" } */
  #pragma omp atomic compare
  --x;					/* { dg-error "invalid form of 'pragma omp atomic compare'" } */
  #pragma omp atomic compare
  x += 3;				/* { dg-error "expected '=' before '\\\+=' token" } */
  #pragma omp atomic compare
  x -= 5;				/* { dg-error "expected '=' before '-=' token" } */
  #pragma omp atomic compare
  x *= 2;				/* { dg-error "expected '=' before '\\\*=' token" } */
  #pragma omp atomic compare
  x |= 5;				/* { dg-error "expected '=' before '\\\|=' token" } */
  #pragma omp atomic compare
  x &= ~5;				/* { dg-error "expected '=' before '\\\&=' token" } */
  #pragma omp atomic compare
  x ^= 5;				/* { dg-error "expected '=' before '\\\^=' token" } */
  #pragma omp atomic compare
  x = x + 3;				/* { dg-error "invalid form of '#pragma omp atomic' before ';' token" "" { target c } } */
  #pragma omp atomic compare		/* { dg-error "invalid form of '#pragma omp atomic compare' before '\\\+' token" "" { target c++ } .-1 } */
  x = x - 5;				/* { dg-error "invalid form of '#pragma omp atomic' before ';' token" "" { target c } } */
  #pragma omp atomic compare		/* { dg-error "invalid form of '#pragma omp atomic compare' before '-' token" "" { target c++ } .-1 } */
  x = 2 * x;				/* { dg-error "invalid form of '#pragma omp atomic' before ';' token" "" { target c } } */
  #pragma omp atomic compare		/* { dg-error "invalid form of '#pragma omp atomic' before numeric constant" "" { target c++ } .-1 } */
  x = 5 | x;				/* { dg-error "invalid form of '#pragma omp atomic' before ';' token" "" { target c } } */
  #pragma omp atomic compare		/* { dg-error "invalid form of '#pragma omp atomic' before numeric constant" "" { target c++ } .-1 } */
  x = x & ~5;				/* { dg-error "invalid form of '#pragma omp atomic' before ';' token" "" { target c } } */
  #pragma omp atomic compare		/* { dg-error "invalid form of '#pragma omp atomic compare' before '\\\&' token" "" { target c++ } .-1 } */
  x = x | 5;				/* { dg-error "invalid form of '#pragma omp atomic' before ';' token" "" { target c } } */
  #pragma omp atomic compare		/* { dg-error "invalid form of '#pragma omp atomic compare' before '\\\|' token" "" { target c++ } .-1 } */
  x = x >= 5 ? 5 : x;			/* { dg-error "invalid form of '#pragma omp atomic' before ';' token" "" { target c } } */
  #pragma omp atomic compare		/* { dg-error "invalid operator for '#pragma omp atomic' before '>=' token" "" { target c++ } .-1 } */
  x = x <= 5 ? 5 : x;			/* { dg-error "invalid form of '#pragma omp atomic' before ';' token" "" { target c } } */
  #pragma omp atomic compare		/* { dg-error "invalid operator for '#pragma omp atomic' before '<=' token" "" { target c++ } .-1 } */
  x = x != 5 ? 7 : x;			/* { dg-error "invalid form of '#pragma omp atomic' before ';' token" "" { target c } } */
  #pragma omp atomic compare		/* { dg-error "invalid operator for '#pragma omp atomic' before '!=' token" "" { target c++ } .-1 } */
  x = 5 == x ? 7 : x;			/* { dg-error "invalid form of '#pragma omp atomic' before ';' token" "" { target c } } */
  #pragma omp atomic compare		/* { dg-error "invalid form of '#pragma omp atomic' before numeric constant" "" { target c++ } .-1 } */
  x = x == 5 ? x : 7;			/* { dg-error "invalid form of '#pragma omp atomic' before ';' token" "" { target c } } */
  #pragma omp atomic compare		/* { dg-error "invalid form of '#pragma omp atomic compare' before ';' token" "" { target c++ } .-1 } */
  x = x == 5 ? 9 : 7;			/* { dg-error "invalid form of '#pragma omp atomic' before ';' token" "" { target c } } */
  #pragma omp atomic compare		/* { dg-error "invalid form of '#pragma omp atomic compare' before ';' token" "" { target c++ } .-1 } */
  x = x > 5 ? 6 : x;			/* { dg-error "invalid form of '#pragma omp atomic' before ';' token" "" { target c } } */
  #pragma omp atomic compare		/* { dg-error "invalid form of '#pragma omp atomic compare' before ';' token" "" { target c++ } .-1 } */
  x = x < 5 ? 6 : x;			/* { dg-error "invalid form of '#pragma omp atomic' before ';' token" "" { target c } } */
  #pragma omp atomic compare		/* { dg-error "invalid form of '#pragma omp atomic compare' before ';' token" "" { target c++ } .-1 } */
  x = x > 5 ? x : 6;			/* { dg-error "invalid form of '#pragma omp atomic' before ';' token" "" { target c } } */
  #pragma omp atomic compare		/* { dg-error "invalid form of '#pragma omp atomic compare' before ';' token" "" { target c++ } .-1 } */
  x = x < 5 ? x : 6;			/* { dg-error "invalid form of '#pragma omp atomic' before ';' token" "" { target c } } */
  #pragma omp atomic capture		/* { dg-error "invalid form of '#pragma omp atomic compare' before ';' token" "" { target c++ } .-1 } */
  r = x == 5;				/* { dg-error "invalid operator for '#pragma omp atomic' before '==' token" } */
  #pragma omp atomic capture compare
  r = x == 5;				/* { dg-error "expected '=' before '==' token" } */
  #pragma omp atomic capture compare	/* { dg-error "'#pragma omp atomic compare capture' with non-integral comparison result" } */
  { v = x == 5; if (v) { x = 6; } }
  #pragma omp atomic compare capture
  { r2 = x; x = y; }			/* { dg-error "invalid form of '#pragma omp atomic' before ';' token" } */
  #pragma omp atomic compare capture
  { r2 = x; x = y == 7 ? 12 : y; }	/* { dg-error "invalid form of '#pragma omp atomic' before ';' token" "" { target c } } */
					/* { dg-error "invalid form of '#pragma omp atomic' before 'y'" "" { target c++ } .-1 } */
}

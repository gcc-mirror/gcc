/* PR 7543. Test operation of -Wparentheses.  Precedence warnings.  
   !a | b and !a & b. */
/* { dg-do compile } */
/* { dg-options "-Wparentheses" } */
// C++ version of Wparentheses-11.c
int foo (int);

void
bar (int a, int b, int c)
{
  foo (!a & b); /* { dg-warning "parentheses" "correct warning" } */
  foo (!a & (b < c));
  foo (!a & (b > c));
  foo (!a & (b == c));
  foo (!a & (b != c));
  foo (!a & (b <= c));
  foo (!a & (b >= c));
  foo (!a & (b && c));
  foo (!a & (b || c));
  foo (!a & !b);
  foo (!(a & b));
  foo ((!a) & b);
  foo (!a & 2); /* { dg-warning "parentheses" "correct warning" } */
  foo (!a & (2 < c));
  foo (!a & (2 > c));
  foo (!a & (2 == c));
  foo (!a & (2 != c));
  foo (!a & (2 <= c));
  foo (!a & (2 >= c));
  foo (!a & (2 && c));
  foo (!a & (2 || c));
  foo (!a & !2);
  foo (!(a & 2));
  foo ((!a) & 2);
  foo (!1 & 2); /* { dg-warning "parentheses" "correct warning" } */
  foo (!1 & (2 < c));
  foo (!1 & (2 > c));
  foo (!1 & (2 == c));
  foo (!1 & (2 != c));
  foo (!1 & (2 <= c));
  foo (!1 & (2 >= c));
  foo (!1 & (2 && c));
  foo (!1 & (2 || c));
  foo (!1 & !2);
  foo (!(1 & 2));

  foo (!a | b); /* { dg-warning "parentheses" "correct warning" } */
  foo (!a | (b < c));
  foo (!a | (b > c));
  foo (!a | (b == c));
  foo (!a | (b != c));
  foo (!a | (b <= c));
  foo (!a | (b >= c));
  foo (!a | (b && c));
  foo (!a | (b || c));
  foo (!a | !b);
  foo (!(a | b));
  foo ((!a) | b);
  foo (!a | 2); /* { dg-warning "parentheses" "correct warning" } */
  foo (!a | (2 < c));
  foo (!a | (2 > c));
  foo (!a | (2 == c));
  foo (!a | (2 != c));
  foo (!a | (2 <= c));
  foo (!a | (2 >= c));
  foo (!a | (2 && c));
  foo (!a | (2 || c));
  foo (!a | !2);
  foo (!(a | 2));
  foo ((!a) | 2);
  foo (!1 | 2); /* { dg-warning "parentheses" "correct warning" } */
  foo (!1 | (2 < c));
  foo (!1 | (2 > c));
  foo (!1 | (2 == c));
  foo (!1 | (2 != c));
  foo (!1 | (2 <= c));
  foo (!1 | (2 >= c));
  foo (!1 | (2 && c));
  foo (!1 | (2 || c));
  foo (!1 | !2);
  foo (!(1 | 2));
  foo ((!1) | 2);

  foo (b & !a); /* { dg-bogus "parentheses" "bogus warning" } */
  foo ((b < c) & !a);
  foo ((b > c) & !a);
  foo ((b == c) & !a);
  foo ((b != c) & !a);
  foo ((b <= c) & !a);
  foo ((b >= c) & !a);
  foo ((b && c) & !a);
  foo ((b || c) & !a);
  foo (!b & !a);
  foo (!(b & a));
  foo (b & (!a));
  foo (2 & !a); /* { dg-bogus "parentheses" "correct warning" } */
  foo ((2 < c) & !a);
  foo ((2 > c) & !a);
  foo ((2 == c) & !a);
  foo ((2 != c) & !a);
  foo ((2 <= c) & !a);
  foo ((2 >= c) & !a);
  foo ((2 && c) & !a);
  foo ((2 || c) & !a);
  foo (!2 & !a);
  foo (!(2 & a));
  foo (2 & (!a));
  foo (2 & !1); /* { dg-bogus "parentheses" "correct warning" } */
  foo ((2 < c) & !1);
  foo ((2 > c) & !1);
  foo ((2 == c) & !1);
  foo ((2 != c) & !1);
  foo ((2 <= c) & !1);
  foo ((2 >= c) & !1);
  foo ((2 && c) & !1);
  foo ((2 || c) & !1);
  foo (!2 & !1);
  foo (!(2 & 1));

  foo (b | !a); /* { dg-bogus "parentheses" "correct warning" } */
  foo ((b < c) | !a);
  foo ((b > c) | !a);
  foo ((b == c) | !a);
  foo ((b != c) | !a);
  foo ((b <= c) | !a);
  foo ((b >= c) | !a);
  foo ((b && c) | !a);
  foo ((b || c) | !a);
  foo (!b | !a);
  foo (!(b | a));
  foo (b | (!a));
  foo (2 | !a); /* { dg-bogus "parentheses" "correct warning" } */
  foo ((2 < c) | !a);
  foo ((2 > c) | !a);
  foo ((2 == c) | !a);
  foo ((2 != c) | !a);
  foo ((2 <= c) | !a);
  foo ((2 >= c) | !a);
  foo ((2 && c) | !a);
  foo ((2 || c) | !a);
  foo (!2 | !a);
  foo (!(2 | a));
  foo (2 | (!a));
  foo (2 | !1); /* { dg-bogus "parentheses" "correct warning" } */
  foo ((2 < c) | !1);
  foo ((2 > c) | !1);
  foo ((2 == c) | !1);
  foo ((2 != c) | !1);
  foo ((2 <= c) | !1);
  foo ((2 >= c) | !1);
  foo ((2 && c) | !1);
  foo ((2 || c) | !1);
  foo (!2 | !1);
  foo (!(2 | 1));
  foo (2 | (!1));
}


void
baz (int a, int b, int c)
{
  foo (!a & (b << c));/* { dg-warning "parentheses" "correct warning" } */
  foo (!a & (b >> c));/* { dg-warning "parentheses" "correct warning" } */
  foo (!a & (b + c)); /* { dg-warning "parentheses" "correct warning" } */
  foo (!a & (b - c)); /* { dg-warning "parentheses" "correct warning" } */
  foo (!a & (b = c)); /* { dg-warning "parentheses" "correct warning" } */
  foo (!a & ~b);      /* { dg-warning "parentheses" "correct warning" } */
  foo (!a & (b & c)); /* { dg-warning "parentheses" "correct warning" } */
  foo (!a & (b | c)); /* { dg-warning "parentheses" "correct warning" } */
  foo (!a & 2);       /* { dg-warning "parentheses" "correct warning" } */
  foo (!a & (2 << c));/* { dg-warning "parentheses" "correct warning" } */
  foo (!a & (2 >> c));/* { dg-warning "parentheses" "correct warning" } */
  foo (!a & (2 + c)); /* { dg-warning "parentheses" "correct warning" } */
  foo (!a & (2 - c)); /* { dg-warning "parentheses" "correct warning" } */
  foo (!a & (c = 2)); /* { dg-warning "parentheses" "correct warning" } */
  foo (!a & ~2);      /* { dg-warning "parentheses" "correct warning" } */
  foo (!a & (2 & c)); /* { dg-warning "parentheses" "correct warning" } */
  foo (!a & (2 | c)); /* { dg-warning "parentheses" "correct warning" } */
  foo (!1 & (2 << c));/* { dg-warning "parentheses" "correct warning" } */
  foo (!1 & (2 >> c));/* { dg-warning "parentheses" "correct warning" } */
  foo (!1 & (2 + c)); /* { dg-warning "parentheses" "correct warning" } */
  foo (!1 & (2 - c)); /* { dg-warning "parentheses" "correct warning" } */
  foo (!1 & (c = 2)); /* { dg-warning "parentheses" "correct warning" } */
  foo (!1 & ~2);      /* { dg-warning "parentheses" "correct warning" } */
  foo (!1 & (2 & c)); /* { dg-warning "parentheses" "correct warning" } */
  foo (!1 & (2 | c)); /* { dg-warning "parentheses" "correct warning" } */
  foo (!a | (b << c));/* { dg-warning "parentheses" "correct warning" } */
  foo (!a | (b >> c));/* { dg-warning "parentheses" "correct warning" } */
  foo (!a | (b + c)); /* { dg-warning "parentheses" "correct warning" } */
  foo (!a | (b - c)); /* { dg-warning "parentheses" "correct warning" } */
  foo (!a | (b = c)); /* { dg-warning "parentheses" "correct warning" } */
  foo (!a | ~b);      /* { dg-warning "parentheses" "correct warning" } */
  foo (!a | (b & c)); /* { dg-warning "parentheses" "correct warning" } */
  foo (!a | (b | c)); /* { dg-warning "parentheses" "correct warning" } */
  foo (!a | (2 << c));/* { dg-warning "parentheses" "correct warning" } */
  foo (!a | (2 >> c));/* { dg-warning "parentheses" "correct warning" } */
  foo (!a | (2 + c)); /* { dg-warning "parentheses" "correct warning" } */
  foo (!a | (2 - c)); /* { dg-warning "parentheses" "correct warning" } */
  foo (!a | (c = 2)); /* { dg-warning "parentheses" "correct warning" } */
  foo (!a | ~2);      /* { dg-warning "parentheses" "correct warning" } */
  foo (!a | (2 & c)); /* { dg-warning "parentheses" "correct warning" } */
  foo (!a | (2 | c)); /* { dg-warning "parentheses" "correct warning" } */
  foo (!1 | (2 << c));/* { dg-warning "parentheses" "correct warning" } */
  foo (!1 | (2 >> c));/* { dg-warning "parentheses" "correct warning" } */
  foo (!1 | (2 + c)); /* { dg-warning "parentheses" "correct warning" } */
  foo (!1 | (2 - c)); /* { dg-warning "parentheses" "correct warning" } */
  foo (!1 | (c = 2)); /* { dg-warning "parentheses" "correct warning" } */
  foo (!1 | ~2);      /* { dg-warning "parentheses" "correct warning" } */
  foo (!1 | (2 & c)); /* { dg-warning "parentheses" "correct warning" } */
  foo (!1 | (2 | c)); /* { dg-warning "parentheses" "correct warning" } */
  foo ((b << c) & !a);
  foo ((b >> c) & !a);
  foo ((b + c) & !a);
  foo ((b - c) & !a);
  foo ((b = c) & !a);
  foo (~b & !a);
  foo ((b & c) & !a);
  foo ((b | c) & !a);
  foo ((2 << c) & !a);
  foo ((2 >> c) & !a);
  foo ((2 + c) & !a);
  foo ((2 - c) & !a);
  foo ((c = 2) & !a);
  foo (~2 & !a);
  foo ((2 & c) & !a);
  foo ((2 | c) & !a);
  foo ((2 << c) & !1);
  foo ((2 >> c) & !1);
  foo ((2 + c) & !1);
  foo ((2 - c) & !1);
  foo ((c = 2) & !1);
  foo (~2 & !1);
  foo ((2 & c) & !1);
  foo ((2 | c) & !1);
  foo ((b << c) | !a);
  foo ((b >> c) | !a);
  foo ((b + c) | !a);
  foo ((b - c) | !a);
  foo ((b = c) | !a);
  foo (~b | !a);
  foo ((b & c) | !a);
  foo ((b | c) | !a);
  foo ((2 << c) | !a);
  foo ((2 >> c) | !a);
  foo ((2 + c) | !a);
  foo ((2 - c) | !a);
  foo ((c = 2) | !a);
  foo (~2 | !a);
  foo ((2 & c) | !a);
  foo ((2 | c) | !a);
  foo ((2 << c) | !1);
  foo ((2 >> c) | !1);
  foo ((2 + c) | !1);
  foo ((2 - c) | !1);
  foo ((c = 2) | !1);
  foo (~2 | !1);
  foo ((2 & c) | !1);
  foo ((2 | c) | !1);
}

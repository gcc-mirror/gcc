/* Test operation of -Wparentheses.  Precedence warnings.  + or - or
   comparison inside &.  */
/* Origin: Joseph Myers <jsm@polyomino.org.uk> */

/* { dg-do compile } */
/* { dg-options "-Wparentheses" } */

int foo (int);

int
bar (int a, int b, int c)
{
  foo (a + b & c); /* { dg-warning "parentheses" "correct warning" } */
  foo ((a + b) & c);
  foo (a + (b & c));
  foo (1 + 2 & c); /* { dg-warning "parentheses" "correct warning" } */
  foo ((1 + 2) & c);
  foo (1 + (2 & c));
  foo (1 + 2 & 3); /* { dg-warning "parentheses" "correct warning" } */
  foo ((1 + 2) & 3);
  foo (1 + (2 & 3));
  foo (a & b + c); /* { dg-warning "parentheses" "correct warning" } */
  foo ((a & b) + c);
  foo (a & (b + c));
  foo (1 & 2 + c); /* { dg-warning "parentheses" "correct warning" } */
  foo ((1 & 2) + c);
  foo (1 & (2 + c));
  foo (1 & 2 + 3); /* { dg-warning "parentheses" "correct warning" } */
  foo ((1 & 2) + 3);
  foo (1 & (2 + 3));
  foo (a - b & c); /* { dg-warning "parentheses" "correct warning" } */
  foo ((a - b) & c);
  foo (a - (b & c));
  foo (1 - 2 & c); /* { dg-warning "parentheses" "correct warning" } */
  foo ((1 - 2) & c);
  foo (1 - (2 & c));
  foo (1 - 2 & 3); /* { dg-warning "parentheses" "correct warning" } */
  foo ((1 - 2) & 3);
  foo (1 - (2 & 3));
  foo (a & b - c); /* { dg-warning "parentheses" "correct warning" } */
  foo ((a & b) - c);
  foo (a & (b - c));
  foo (1 & 2 - c); /* { dg-warning "parentheses" "correct warning" } */
  foo ((1 & 2) - c);
  foo (1 & (2 - c));
  foo (1 & 2 - 3); /* { dg-warning "parentheses" "correct warning" } */
  foo ((1 & 2) - 3);
  foo (1 & (2 - 3));
  foo (a < b & c); /* { dg-warning "parentheses" "correct warning" } */
  foo ((a < b) & c);
  foo (a < (b & c));
  foo (1 < 2 & c); /* { dg-warning "parentheses" "correct warning" } */
  foo ((1 < 2) & c);
  foo (1 < (2 & c));
  foo (1 < 2 & 3); /* { dg-warning "parentheses" "correct warning" } */
  foo ((1 < 2) & 3);
  foo (1 < (2 & 3));
  foo (a & b < c); /* { dg-warning "parentheses" "correct warning" } */
  foo ((a & b) < c);
  foo (a & (b < c));
  foo (1 & 2 < c); /* { dg-warning "parentheses" "correct warning" } */
  foo ((1 & 2) < c);
  foo (1 & (2 < c));
  foo (1 & 2 < 3); /* { dg-warning "parentheses" "correct warning" } */
  foo ((1 & 2) < 3);
  foo (1 & (2 < 3));
  foo (a == b & c); /* { dg-warning "parentheses" "correct warning" } */
  foo ((a == b) & c);
  foo (a == (b & c));
  foo (1 == 2 & c); /* { dg-warning "parentheses" "correct warning" } */
  foo ((1 == 2) & c);
  foo (1 == (2 & c));
  foo (1 == 2 & 3); /* { dg-warning "parentheses" "correct warning" } */
  foo ((1 == 2) & 3);
  foo (1 == (2 & 3));
  foo (a & b == c); /* { dg-warning "parentheses" "correct warning" } */
  foo ((a & b) == c);
  foo (a & (b == c));
  foo (1 & 2 == c); /* { dg-warning "parentheses" "correct warning" } */
  foo ((1 & 2) == c);
  foo (1 & (2 == c));
  foo (1 & 2 == 3); /* { dg-warning "parentheses" "correct warning" } */
  foo ((1 & 2) == 3);
  foo (1 & (2 == 3));
  foo (a != b & c); /* { dg-warning "parentheses" "correct warning" } */
  foo ((a != b) & c);
  foo (a != (b & c));
  foo (1 != 2 & c); /* { dg-warning "parentheses" "correct warning" } */
  foo ((1 != 2) & c);
  foo (1 != (2 & c));
  foo (1 != 2 & 3); /* { dg-warning "parentheses" "correct warning" } */
  foo ((1 != 2) & 3);
  foo (1 != (2 & 3));
  foo (a & b != c); /* { dg-warning "parentheses" "correct warning" } */
  foo ((a & b) != c);
  foo (a & (b != c));
  foo (1 & 2 != c); /* { dg-warning "parentheses" "correct warning" } */
  foo ((1 & 2) != c);
  foo (1 & (2 != c));
  foo (1 & 2 != 3); /* { dg-warning "parentheses" "correct warning" } */
  foo ((1 & 2) != 3);
  foo (1 & (2 != 3));
}

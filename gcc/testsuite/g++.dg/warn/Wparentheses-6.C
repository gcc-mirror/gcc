// { dg-do compile }
// { dg-options "-Wparentheses" }

// C++ version of gcc.dg/Wparentheses-2.c.

int foo (int);

int
bar (int a, int b, int c)
{
  foo (a <= b <= c); // { dg-warning "comparison" "correct warning" }
  foo ((a <= b) <= c);
  foo (a <= (b <= c));
  foo (1 <= 2 <= c); // { dg-warning "comparison" "correct warning" }
  foo ((1 <= 2) <= c);
  foo (1 <= (2 <= c));
  foo (1 <= 2 <= 3); // { dg-warning "comparison" "correct warning" }
  foo ((1 <= 2) <= 3);
  foo (1 <= (2 <= 3));
  foo (a > b > c); // { dg-warning "comparison" "correct warning" }
  foo ((a > b) > c);
  foo (a > (b > c));
  foo (1 > 2 > c); // { dg-warning "comparison" "correct warning" }
  foo ((1 > 2) > c);
  foo (1 > (2 > c));
  foo (1 > 2 > 3); // { dg-warning "comparison" "correct warning" }
  foo ((1 > 2) > 3);
  foo (1 > (2 > 3));
  foo (a < b <= c); // { dg-warning "comparison" "correct warning" }
  foo ((a < b) <= c);
  foo (a < (b <= c));
  foo (1 < 2 <= c); // { dg-warning "comparison" "correct warning" }
  foo ((1 < 2) <= c);
  foo (1 < (2 <= c));
  foo (1 < 2 <= 3); // { dg-warning "comparison" "correct warning" }
  foo ((1 < 2) <= 3);
  foo (1 < (2 <= 3));
  foo (a <= b > c); // { dg-warning "comparison" "correct warning" }
  foo ((a <= b) > c);
  foo (a <= (b > c));
  foo (1 <= 2 > c); // { dg-warning "comparison" "correct warning" }
  foo ((1 <= 2) > c);
  foo (1 <= (2 > c));
  foo (1 <= 2 > 3); // { dg-warning "comparison" "correct warning" }
  foo ((1 <= 2) > 3);
  foo (1 <= (2 > 3));
  foo (a <= b == c); // { dg-warning "comparison" "correct warning" }
  foo ((a <= b) == c);
  foo (a <= (b == c));
  foo (1 <= 2 == c); // { dg-warning "comparison" "correct warning" }
  foo ((1 <= 2) == c);
  foo (1 <= (2 == c));
  foo (1 <= 2 == 3); // { dg-warning "comparison" "correct warning" }
  foo ((1 <= 2) == 3);
  foo (1 <= (2 == 3));
  foo (a != b != c); // { dg-warning "comparison" "correct warning" }
  foo ((a != b) != c);
  foo (a != (b != c));
  foo (1 != 2 != c); // { dg-warning "comparison" "correct warning" }
  foo ((1 != 2) != c);
  foo (1 != (2 != c));
  foo (1 != 2 != 3); // { dg-warning "comparison" "correct warning" }
  foo ((1 != 2) != 3);
  foo (1 != (2 != 3));
}

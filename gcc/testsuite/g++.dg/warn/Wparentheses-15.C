// { dg-do compile }
// { dg-options "-Wparentheses" }

// Template version of Wparentheses-6.C.

int foo (int);

template<class T>
void
bar (T a, T b, T c)
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

template void bar<int> (int, int, int); // { dg-warning "instantiated" }

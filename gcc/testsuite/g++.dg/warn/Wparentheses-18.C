// { dg-do compile }
// { dg-options "-Wparentheses" }

// Template version of Wparentheses-9.C.

int foo (int);

template<class T>
void
bar (T a, T b, T c)
{
  foo (a & b | c); // { dg-warning "parentheses" "correct warning" }
  foo ((a & b) | c);
  foo (a & (b | c));
  foo (1 & 2 | c); // { dg-warning "parentheses" "correct warning" }
  foo ((1 & 2) | c);
  foo (1 & (2 | c));
  foo (1 & 2 | 3); // { dg-warning "parentheses" "correct warning" }
  foo ((1 & 2) | 3);
  foo (1 & (2 | 3));
  foo (a | b & c); // { dg-warning "parentheses" "correct warning" }
  foo ((a | b) & c);
  foo (a | (b & c));
  foo (1 | 2 & c); // { dg-warning "parentheses" "correct warning" }
  foo ((1 | 2) & c);
  foo (1 | (2 & c));
  foo (1 | 2 & 3); // { dg-warning "parentheses" "correct warning" }
  foo ((1 | 2) & 3);
  foo (1 | (2 & 3));
  foo (a ^ b | c); // { dg-warning "parentheses" "correct warning" }
  foo ((a ^ b) | c);
  foo (a ^ (b | c));
  foo (1 ^ 2 | c); // { dg-warning "parentheses" "correct warning" }
  foo ((1 ^ 2) | c);
  foo (1 ^ (2 | c));
  foo (1 ^ 2 | 3); // { dg-warning "parentheses" "correct warning" }
  foo ((1 ^ 2) | 3);
  foo (1 ^ (2 | 3));
  foo (a | b ^ c); // { dg-warning "parentheses" "correct warning" }
  foo ((a | b) ^ c);
  foo (a | (b ^ c));
  foo (1 | 2 ^ c); // { dg-warning "parentheses" "correct warning" }
  foo ((1 | 2) ^ c);
  foo (1 | (2 ^ c));
  foo (1 | 0x2 ^ 3); // { dg-warning "parentheses" "correct warning" }
  foo ((1 | 2) ^ 3);
  foo (1 | (0x2 ^ 3));
  foo (a + b | c); // { dg-warning "parentheses" "correct warning" }
  foo ((a + b) | c);
  foo (a + (b | c));
  foo (1 + 2 | c); // { dg-warning "parentheses" "correct warning" }
  foo ((1 + 2) | c);
  foo (1 + (2 | c));
  foo (1 + 2 | 3); // { dg-warning "parentheses" "correct warning" }
  foo ((1 + 2) | 3);
  foo (1 + (2 | 3));
  foo (a | b + c); // { dg-warning "parentheses" "correct warning" }
  foo ((a | b) + c);
  foo (a | (b + c));
  foo (1 | 2 + c); // { dg-warning "parentheses" "correct warning" }
  foo ((1 | 2) + c);
  foo (1 | (2 + c));
  foo (1 | 2 + 3); // { dg-warning "parentheses" "correct warning" }
  foo ((1 | 2) + 3);
  foo (1 | (2 + 3));
  foo (a - b | c); // { dg-warning "parentheses" "correct warning" }
  foo ((a - b) | c);
  foo (a - (b | c));
  foo (1 - 2 | c); // { dg-warning "parentheses" "correct warning" }
  foo ((1 - 2) | c);
  foo (1 - (2 | c));
  foo (1 - 2 | 3); // { dg-warning "parentheses" "correct warning" }
  foo ((1 - 2) | 3);
  foo (1 - (2 | 3));
  foo (a | b - c); // { dg-warning "parentheses" "correct warning" }
  foo ((a | b) - c);
  foo (a | (b - c));
  foo (1 | 2 - c); // { dg-warning "parentheses" "correct warning" }
  foo ((1 | 2) - c);
  foo (1 | (2 - c));
  foo (1 | 2 - 3); // { dg-warning "parentheses" "correct warning" }
  foo ((1 | 2) - 3);
  foo (1 | (2 - 3));
  foo (a > b | c); // { dg-warning "parentheses" "correct warning" }
  foo ((a > b) | c);
  foo (a > (b | c));
  foo (1 > 2 | c); // { dg-warning "parentheses" "correct warning" }
  foo ((1 > 2) | c);
  foo (1 > (2 | c));
  foo (1 > 2 | 3); // { dg-warning "parentheses" "correct warning" }
  foo ((1 > 2) | 3);
  foo (1 > (2 | 3));
  foo (a | b > c); // { dg-warning "parentheses" "correct warning" }
  foo ((a | b) > c);
  foo (a | (b > c));
  foo (1 | 2 > c); // { dg-warning "parentheses" "correct warning" }
  foo ((1 | 2) > c);
  foo (1 | (2 > c));
  foo (1 | 2 > 3); // { dg-warning "parentheses" "correct warning" }
  foo ((1 | 2) > 3);
  foo (1 | (2 > 3));
  foo (a <= b | c); // { dg-warning "parentheses" "correct warning" }
  foo ((a <= b) | c);
  foo (a <= (b | c));
  foo (1 <= 2 | c); // { dg-warning "parentheses" "correct warning" }
  foo ((1 <= 2) | c);
  foo (1 <= (2 | c));
  foo (1 <= 2 | 3); // { dg-warning "parentheses" "correct warning" }
  foo ((1 <= 2) | 3);
  foo (1 <= (2 | 3));
  foo (a | b <= c); // { dg-warning "parentheses" "correct warning" }
  foo ((a | b) <= c);
  foo (a | (b <= c));
  foo (1 | 2 <= c); // { dg-warning "parentheses" "correct warning" }
  foo ((1 | 2) <= c);
  foo (1 | (2 <= c));
  foo (1 | 2 <= 3); // { dg-warning "parentheses" "correct warning" }
  foo ((1 | 2) <= 3);
  foo (1 | (2 <= 3));
}

template void bar<int> (int, int, int); // { dg-message "required" }

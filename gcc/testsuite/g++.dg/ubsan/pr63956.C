// PR sanitizer/63956
// { dg-do compile }
// { dg-options "-std=c++14 -fsanitize=undefined,float-divide-by-zero,float-cast-overflow" }

#define SA(X) static_assert((X),#X)
#define INT_MIN (-__INT_MAX__ - 1)

constexpr int
fn1 (int a, int b)
{
  if (b != 2)
    a <<= b;
    // { dg-error "5 << -2.. is negative" "" { target *-*-* } .-1 }
    // { dg-error "is >= than the precision of the left operand" "" { target *-*-* } .-2 }
    // { dg-error "-2 << 4.. is negative" "" { target *-*-* } .-3 }
  return a;
}

constexpr int i1 = fn1 (5, 3);
constexpr int i2 = fn1 (5, -2); // { dg-message "in constexpr expansion" }
constexpr int i3 = fn1 (5, sizeof (int) * __CHAR_BIT__); // { dg-message "in constexpr expansion" }
constexpr int i4 = fn1 (5, 256); // { dg-message "in constexpr expansion" }
constexpr int i5 = fn1 (5, 2);
constexpr int i6 = fn1 (-2, 4); // { dg-message "in constexpr expansion" }
constexpr int i7 = fn1 (0, 2);

SA (i1 == 40);
SA (i5 == 5);
SA (i7 == 0);

constexpr int
fn2 (int a, int b)
{
  if (b != 2)
    a >>= b;
    // { dg-error "4 >> -1.. is negative" "" { target *-*-* } .-1 }
    // { dg-error "is >= than the precision of the left operand" "" { target *-*-* } .-2 }

  return a;
}

constexpr int j1 = fn2 (4, 1);
constexpr int j2 = fn2 (4, -1); // { dg-message "in constexpr expansion" }
constexpr int j3 = fn2 (10, sizeof (int) * __CHAR_BIT__); // { dg-message "in constexpr expansion" }
constexpr int j4 = fn2 (1, 256); // { dg-message "in constexpr expansion" }
constexpr int j5 = fn2 (5, 2);
constexpr int j6 = fn2 (-2, 4);
constexpr int j7 = fn2 (0, 4);

SA (j1 == 2);
SA (j5 == 5);
SA (j7 == 0);

constexpr int
fn3 (int a, int b)
{
  if (b != 2)
    a = a / b; // { dg-error "..7 / 0.. is not a constant expression" }
  return a;
}

constexpr int k1 = fn3 (8, 4);
constexpr int k2 = fn3 (7, 0); // { dg-message "in constexpr expansion" }
constexpr int k3 = fn3 (INT_MIN, -1); // { dg-error "overflow in constant expression" }

SA (k1 == 2);

constexpr float
fn4 (float a, float b)
{
  if (b != 2.0)
    a = a / b; // { dg-error "is not a constant expression" }
  return a;
}

constexpr float l1 = fn4 (5.0, 3.0);
constexpr float l2 = fn4 (7.0, 0.0); // { dg-message "in constexpr expansion" }

constexpr int
fn5 (const int *a, int b)
{
  if (b != 2)
    b = a[b];
  return b;
}

constexpr int m1[4] = { 1, 2, 3, 4 };
constexpr int m2 = fn5 (m1, 3);
constexpr int m3 = fn5 (m1, 4); // { dg-error "array subscript" }

constexpr int
fn6 (const int &a, int b)
{
  if (b != 2)
    b = a;
  return b;
}

constexpr int
fn7 (const int *a, int b)
{
  if (b != 3)
    return fn6 (*a, b);
  return 7;
}

constexpr int n1 = 7;
constexpr int n2 = fn7 (&n1, 5);
constexpr int n3 = fn7 ((const int *) 0, 8);  // { dg-error "null pointer" }

constexpr int
fn8 (int i)
{
  constexpr int g[10] = { };
  return g[i];
}

constexpr int o1 = fn8 (9);
constexpr int o2 = fn8 (10); // { dg-error "array subscript" }

constexpr int
fn9 (int a, int b)
{
  if (b != 0)
    return a + b;
  return a;
}

constexpr int p1 = fn9 (42, 7);
constexpr int p2 = fn9 (__INT_MAX__, 1); // { dg-error "overflow in constant expression" }
constexpr int p3 = fn9 (__INT_MAX__, -1);
constexpr int p4 = fn9 (INT_MIN, 1);
constexpr int p5 = fn9 (INT_MIN, -1); // { dg-error "overflow in constant expression" }

SA (p1 == 49);
SA (p3 == __INT_MAX__ - 1);
SA (p4 == INT_MIN + 1);

constexpr int
fn10 (int a, int b)
{
  if (b != 0)
    return a * b;
  return a;
}

constexpr int q1 = fn10 (10, 10);
constexpr int q2 = fn10 (__INT_MAX__, 2); // { dg-error "overflow in constant expression" }
constexpr int q3 = fn10 (INT_MIN, 2); // { dg-error "overflow in constant expression" }
constexpr int q4 = fn10 (-1, -1);

SA (q1 == 100);
SA (q4 == 1);

constexpr int
fn11 (double d)
{
  int i = d;
  if (i != 0)
    return i;
  return i * 2;
}

constexpr int r1 = fn11 (3.4);
constexpr int r2 = fn11 (__builtin_inf ()); // { dg-error "overflow in constant expression" }

constexpr int
fn12 (int i)
{
  if (i == 42)
    __builtin_unreachable (); // { dg-error "is not a constant expression" }
  return i + 10;
}

constexpr int s1 = fn12 (1);
constexpr int s2 = fn12 (42);

SA (s1 == 11);

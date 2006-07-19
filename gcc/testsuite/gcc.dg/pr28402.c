/* { dg-options "" } */
typedef long long ll;
typedef unsigned long long ull;

int global;

#define A(BASE, OP, AMT) \
  ll BASE ## AMT (ll x) { return x OP AMT; } \
  ull BASE ## AMT ## u (ull x) { return x OP AMT; }

#define B(BASE, OP) \
  A (BASE, OP, 1) \
  A (BASE, OP, 10) \
  A (BASE, OP, 31) \
  A (BASE, OP, 33) \
  A (BASE, OP, 61) \
  A (BASE, OP, global)

B (left, <<)
B (right, >>)

/* { dg-final { scan-assembler-not "__\[a-z\]*si3" } } */

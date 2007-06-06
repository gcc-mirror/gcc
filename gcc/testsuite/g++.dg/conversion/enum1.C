// { dg-do run }
// { dg-options "-O2 -finline-functions" }

enum E { V = 1 };
static const E E_MIN = V;
static const E E_MAX = V;

bool valid(E v) { return v >= E_MIN && v <= E_MAX; }

int main() { return valid(E(2)); }

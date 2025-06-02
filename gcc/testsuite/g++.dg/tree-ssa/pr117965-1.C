// { dg-do compile }
// { dg-options "-O2 -fdump-tree-phiopt1" }

void f();
void f(int&);

static inline const int &
clamp(const int &v, const int &min, const int &max)
{
  const int &t = (v > min) ? v : min;
  return t > max ? max : t;
}

void clamp2(int num1) {
  try {
  int low = -12, high = 12;
  f();
  num1 = clamp(num1, low, high);
  f(num1);
  } catch(...)
  {
       __builtin_printf("caught.\n");
        return;
  }
}

// { dg-final { scan-tree-dump-times "MAX" 1 "phiopt1" } } */
// { dg-final { scan-tree-dump-times "MIN" 1 "phiopt1" } } */

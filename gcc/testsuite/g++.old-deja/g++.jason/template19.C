// Make sure type deduction isn't confused by top-level cv-quals.
template <class T> T max (const T a, const T b)  { return a>b?a:b; }

int main()
{
  int a = 0, b = 1;
  int c = max (a, b);
  int d = max ((const int)a, (const int)b);
}

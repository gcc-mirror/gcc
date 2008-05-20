typedef unsigned long long ull;
ull bar (void);
void foo (ull *x)
{
  ull y = bar ();
  *x += y >> 32;
}

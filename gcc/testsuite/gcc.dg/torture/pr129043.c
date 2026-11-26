__attribute__((noipa)) int
f (int n)
{
  double x = __builtin_nan ("");
  int i;
  for (i = 0; i < n; i++)
    if (x >= x)          /* never true: x is NaN */
      break;
  return i;
}

int main ()
{
  int r = f(5);
  if (r != 5)
    __builtin_abort ();
  return 0;
} 

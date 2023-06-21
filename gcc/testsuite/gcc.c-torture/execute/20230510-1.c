/* This code shows up in worse_state in ipa-pure-const.cc:
   *looping = MAX (*looping, looping2);
   was miscompiling it as just `return 1` though instead of
   `MAX_EXPR<*a, b>` (which should be transformed into `*a | b`
   note MAX_EXPR<bool, bool> is really `bool | bool` so we
   use that to compare against here.
 */
#define bool _Bool
bool __attribute__((noipa)) f(bool *a, bool b)
{
  bool t = *a;
  if (t <= b)
    return b;
  return t;
}
bool __attribute__((noipa)) f1(bool *a, bool b)
{
  return *a | b;
}

int main()
{
  int i = 0;
  int j = 0;

  for (i = 0; i <= 1; i++)
    for (j = 0; j <= 1; j++)
      {
        bool a = i;
        if (f(&a, j) != f1(&a, j))
          __builtin_abort();
      }
  return 0;
}

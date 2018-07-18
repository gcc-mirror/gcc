/* PR 79800 - wrong snprintf result range with precision in a narrow
   negative-positive range
   { dg-do "run" { target c99_runtime } }
   { dg-options "-O2 -Wall" } */

#define FMT "%.*a"
char fmt[] = FMT;

volatile double x = 1.23456789;

void f (int p)
{
  if (p < -1 || 0 < p)
    p = -1;

  char d[30];
  int n1 = __builtin_sprintf (d, "%.*a", p, x);
  const char *s = n1 < 20 ? "< 20" : ">= 20";

  if (__builtin_strcmp (s, ">= 20"))
    __builtin_abort ();
}

volatile int i = -1;

int main ()
{
  f (i);

  return 0;
}

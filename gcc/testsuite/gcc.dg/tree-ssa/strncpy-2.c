/* PR tree-optimization/83075 - Invalid strncpy optimization
   { dg-do run }
   { dg-options "-O2 -Wno-stringop-overflow" } */

int main (void)
{
  char a[8] = "";

  __builtin_strcpy (a, "123");

  unsigned n0 = __builtin_strlen (a);

  __builtin_strncpy (a + 3, a, n0);

  unsigned n1 = __builtin_strlen (a);

  if (n1 == n0)
    __builtin_abort ();
}

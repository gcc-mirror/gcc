/* { dg-do compile } */
/* { dg-options "-O2 -Warray-bounds" } */

int main()
{
  unsigned i, j, a[10] = {0};

  for (j = 23; j < 25; j++){
      for (i = j / 8; i --> 0;) a[i] = 0; /* { dg-bogus "array bounds" } */
      for (i =     1; i --> 0;) __builtin_printf("%u", a[i]);
  }

  return 0;
}


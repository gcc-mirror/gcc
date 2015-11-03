/* { dg-do run } */
/* { dg-options "-O3" } */

int a, b = 10;
char c;

int
main ()
{
  char d;
  int e = 5;
  for (a = 0; a; a--)
    e = 0;
  c = (b & 15) ^ e;
  d = c > e ? c : c << e;
  __builtin_printf ("%d\n", d);
  return 0;
}

/* { dg-output "15" } */

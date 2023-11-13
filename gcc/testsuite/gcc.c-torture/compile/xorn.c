/* { dg-additional-options "-std=gnu89" } */

int
xorn (a, b)
     int a, b;
{
  return a ^ ~b;
}

int
not (a)
     int a;
{
  return ~a;
}

int
xor (a, b)
     int a, b;
{
  return a ^ b;
}

main ()
{
  int i, j;

  for (i = 0;  i <= 1;  i++)
    for (j = 0;  j <= 1;  j++)
      printf ("%d op %d = %d = %d?\n", i, j,
	      1 & xor (i, not (j)),
	      1 & xorn (i, j));
}

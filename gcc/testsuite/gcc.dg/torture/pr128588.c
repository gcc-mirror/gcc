/* { dg-do run } */

/* (x not in [113, 115]) | (x != 120) holds for every x.  */

__attribute__((noipa)) int
f (int x)
{
  if (x == 1) return 1;
  if (x == 2) return 1;
  if (x == 3) return 1;
  if (x == 4) return 1;
  if (x == 5) return 1;
  if (x == 6) return 1;
  if (x == 7) return 1;
  if (x == 8) return 1;
  if (x == 9) return 1;
  if (x == 10) return 1;
  if (x == 11) return 1;
  if (x == 12) return 1;
  _Bool a = (unsigned) x - 113u > 2u;
  _Bool b = x != 120;
  if (a | b)
    return 8;
  return 9;
}

int
main (void)
{
  if (f (120) != 8 || f (114) != 8)
    __builtin_abort ();
  return 0;
}

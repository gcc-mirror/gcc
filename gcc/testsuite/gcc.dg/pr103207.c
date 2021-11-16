// { dg-do compile }
// { dg-options "-O2 --param case-values-threshold=1 -w" }

int f (int i)
{
  switch (i) {
  case 2147483647:
    return 1;
  case 9223372036854775807L:
    return 2;
  case (2147483647*4)%4:
    return 4;
  }
  return 0;
}

/* { dg-do compile } */

short int
ix (int *ld, short int oi)
{
  *ld = ((unsigned short int)oi | oi) && !!(*ld);
  return (oi != 0) ? oi : 1;
}

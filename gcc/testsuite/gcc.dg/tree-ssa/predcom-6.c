/* { dg-do run } */

int a, c, e[5][2]; 
unsigned int d;

int
main ()
{
  for (d = 0; d < 2; d++)
    if (a ? 0 : e[c + 3][d] & e[c + 4][d])
      break;
  return 0;
}

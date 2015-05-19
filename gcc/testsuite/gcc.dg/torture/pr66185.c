/* { dg-do compile } */

unsigned int a;
int b[5], c;

int
main ()
{
  for (c = 0; c < 4; c++)
    b[c] = b[c+1] > ((b[0] > 0) > a);

  return 0;
}

/* { dg-do run } */

char a;
unsigned short b;
int c, d;
unsigned char e;

int
main ()
{
  int f = 1, g = ~a;
  if (b > f)
    {
      e = b; 
      d = b | e; 
      g = 0;
    }
  c = 1 % g;
  return 0; 
}

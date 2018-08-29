/* { dg-do compile } */

int a, b[2], c, d;

void fn1 ()
{ 
  for (; d < 2; d++)
    { 
      b[d] = a;
      a = c;
    }
}

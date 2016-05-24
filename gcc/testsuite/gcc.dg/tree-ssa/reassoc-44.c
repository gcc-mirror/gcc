/* { dg-do compile } */
/* { dg-options "-O2" } */

unsigned int a;
int b, c;
void fn1 ()
{
  b = a + c + c;
}

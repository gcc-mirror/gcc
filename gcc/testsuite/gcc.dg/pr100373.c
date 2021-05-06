/* { dg-do compile } */
/* { dg-options "-O2 -fcompare-debug" } */

int a;
_Thread_local int b;
void c()
{
  long d = b;
  a = 0;
  b = 0;
}

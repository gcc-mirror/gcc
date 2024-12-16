/* { dg-do compile { target { int32plus } } } */
/* { dg-options "-O -fnon-call-exceptions -favoid-store-forwarding -fno-forward-propagate -finstrument-functions" } */

char *p;
int y;
long x;

void foo()
{
  x /= *(int *)__builtin_memmove(&y, 4 + p, 3);
}

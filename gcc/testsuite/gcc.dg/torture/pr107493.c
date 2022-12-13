/* { dg-do run { target int32plus } } */

int a;
int b(int c) { return c; }
int main()
{
  a = -21;
  for (; a <= 0; a = (unsigned short)(b(a + 2) + 8))
    ;
  if (a != 65525)
    __builtin_abort();
  return 0;
}

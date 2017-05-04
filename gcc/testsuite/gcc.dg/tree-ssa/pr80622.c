/* { dg-do run } */
/* { dg-options "-O" } */

struct S { int d; char e; int f; char g; } a;
char c;

int
main ()
{
  struct S b[][1] = {3, 0, 3, 4, 3, 0, 3, 4, 3, 0, 3, 4, 3, 0, 3, 4, 3,
                      0, 3, 4, 3, 0, 3, 4, 3, 0, 3, 4, 3, 0, 3, 4, 3, 0,
                      3, 4, 3, 4, 7, 7, 3, 5, 0, 3, 4, 7, 7, 3, 5, 0, 3,
                      4, 3, 4, 7, 7, 3, 5, 0, 3, 4, 7, 7, 3, 5, 0, 3, 4};
  a = b[4][0];
  c = b[4][0].e;
  if (a.g != 4)
    __builtin_abort ();
  return 0;
}

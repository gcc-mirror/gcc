/* PR c/119001 */
/* { dg-do run } */
/* { dg-options "" } */

union U { char a[]; int i; };
union U u = { "12345" };
union U v = { .a = "6789" };
union U w = { { 1, 2, 3, 4, 5, 6 } };
union U x = { .a = { 7, 8, 9 } };
union V { int i; char a[]; };
union V y = { .a = "abcdefghijk" };
union V z = { .a = { 10, 11, 12, 13, 14, 15, 16, 17 } };

int
main ()
{
  for (int i = 0; i < 6; ++i)
    if (u.a[i] != "12345"[i])
      __builtin_abort ();
  for (int i = 0; i < 5; ++i)
    if (v.a[i] != "6789"[i])
      __builtin_abort ();
  for (int i = 0; i < 6; ++i)
    if (w.a[i] != i + 1)
      __builtin_abort ();
  for (int i = 0; i < 3; ++i)
    if (x.a[i] != i + 7)
      __builtin_abort ();
  for (int i = 0; i < 12; ++i)
    if (y.a[i] != "abcdefghijk"[i])
      __builtin_abort ();
  for (int i = 0; i < 8; ++i)
    if (z.a[i] != i + 10)
      __builtin_abort ();
}

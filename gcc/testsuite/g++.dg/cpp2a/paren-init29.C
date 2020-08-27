// PR c++/92812
// P1975R0
// { dg-do run { target c++20 } }

int (&&r)[3] = static_cast<int[3]>(42);
int (&&r2)[1] = static_cast<int[]>(42);

int
main ()
{
  if (r[0] != 42 || r[1] != 0 || r[2] != 0)
    __builtin_abort ();
  if (r2[0] != 42 || sizeof (r2) != sizeof (int))
    __builtin_abort ();
}

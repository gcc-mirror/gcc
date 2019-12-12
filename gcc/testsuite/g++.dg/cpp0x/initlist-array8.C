// PR c++/69531 - DR 1307, Overload resolution based on size of array init-list.
// { dg-do run { target c++2a } }

int f(int (&)[1][1]) { return 1; }
int f(int (&)[1][2]) { return 2; }

int g(int (&&)[2][1]) { return 1; }
int g(int (&&)[2][2]) { return 2; }

int h(int (&&)[][1]) { return 1; }
int h(int (&&)[][2]) { return 2; }

int
main ()
{
  int arr1[1][1];
  int arr2[1][2];

  if (f(arr1) != 1)
    __builtin_abort ();
  if (f(arr2) != 2)
    __builtin_abort ();

  if (g({ { 1, 2 }, { 3 } }) != 2)
    __builtin_abort ();

  if (g({ { 1, 2 }, { 3, 4 } }) != 2)
    __builtin_abort ();

  if (h({ { 1, 2 }, { 3 } }) != 2)
    __builtin_abort ();

  if (h({ { 1, 2 }, { 3, 4 } }) != 2)
    __builtin_abort ();
}

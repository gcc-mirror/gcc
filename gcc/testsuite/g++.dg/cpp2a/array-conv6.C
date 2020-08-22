// PR c++/91364 - Implement P0388R4: Permit conversions to arrays of unknown bound.
// { dg-do run { target c++20 } }

// Ranking of reference initialization conversions

int f1(const int(&)[]) { return 1; }
int f1(const int(&)[1]) { return 2; }

int f2(const int(&)[]) { return 1; }
int f2(int(&)[1]) { return 2; }

int f3(int(&)[]) { return 1; }
int f3(const int(&)[1]) { return 2; }

const int arr[1] = { 42 };

int
main ()
{
  if (f1(arr) != 2)
    __builtin_abort ();

  if (f2(arr) != 1)
    __builtin_abort ();

  if (f3(arr) != 2)
    __builtin_abort ();
}

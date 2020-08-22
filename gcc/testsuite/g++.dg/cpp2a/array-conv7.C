// PR c++/69531 - DR 1307, Overload resolution based on size of array init-list.
// { dg-do run { target c++20 } }

int f(int const(&)[]) { return 1; }
int f(int const(&)[2]) { return 2; }

int f2(int const(&)[]) { return 1; }
int f2(int const(&)[1]) { return 2; }

int f3(int const(&)[]) { return 1; }
int f3(int const(&)[1]) { return 2; }
int f3(int const(&)[2]) { return 3; }

int main ()
{
  if (f ({}) != 1)
    __builtin_abort ();
  if (f ({1}) != 1)
    __builtin_abort ();
  if (f ({1, 2}) != 2)
    __builtin_abort ();

  if (f2 ({}) != 1)
    __builtin_abort ();
  if (f2 ({1}) != 2)
    __builtin_abort ();

  if (f3 ({}) != 1)
    __builtin_abort ();
  if (f3 ({1}) != 2)
    __builtin_abort ();
  if (f3 ({1, 2}) != 3)
    __builtin_abort ();
}

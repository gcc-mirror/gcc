// PR c++/69531 - DR 1307, Overload resolution based on size of array init-list.
// { dg-do run { target c++2a } }

int f(int, int const(&)[2]) { return 1; }
int f(double, int const(&)[2]) { return 2; }

int f2(int, int const(&)[1]) { return 1; }
int f2(int, int const(&)[2]) { return 2; }

int f3(int, int const(&)[]) { return 1; }
int f3(double, int const(&)[]) { return 2; }

int main ()
{
  if (f (1, {1}) != 1)
    __builtin_abort ();

  if (f2 (1, {1}) != 1)
    __builtin_abort ();

  if (f3 (1, {1}) != 1)
    __builtin_abort ();
}

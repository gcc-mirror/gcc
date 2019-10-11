// PR c++/69531 - DR 1307, Overload resolution based on size of array init-list.
// { dg-do run { target c++11 } }

int f(int const(&)[2]) { return 1; }
int f(int const(&)[3]) { return 2; }

int
main ()
{
   if (f({}) != 1)
    __builtin_abort ();

   if (f({1}) != 1)
    __builtin_abort ();

   if (f({1, 2}) != 1)
    __builtin_abort ();

   if (f({1, 2, 3}) != 2)
    __builtin_abort ();
}

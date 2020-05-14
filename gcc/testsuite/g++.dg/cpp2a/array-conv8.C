// PR c++/69531 - DR 1307, Overload resolution based on size of array init-list.
// { dg-do run { target c++20 } }
// Example from [over.ics.rank].

int f(int    (&&)[] ) { return 1; }    // #1
int f(double (&&)[] ) { return 2; }    // #2
int f(int    (&&)[2]) { return 3; }    // #3

int
main ()
{
  // Calls #1: Better than #2 due to conversion, better than #3 due to bounds.
  if (f({1}) != 1)
     __builtin_abort ();
  // Calls #2: Identity conversion is better than floating-integral conversion.
  if (f({1.0}) != 2)
     __builtin_abort ();
  // Calls #2: Identity conversion is better than floating-integral conversion.
  if (f({1.0, 2.0}) != 2)
     __builtin_abort ();
  // Calls #3: Converting to array of known bound is better than to unknown
  // bound, and an identity conversion is better than floating-integral
  // conversion.
  if (f({1, 2}) != 3)
     __builtin_abort ();
}

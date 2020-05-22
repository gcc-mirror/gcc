// { dg-do compile }

// Crash tests from PR middle-end/6994.  See also gcc.dg/vla-2.c.
// A::A is acceptable extended C++ (VLA types brought over from C99);
// B::B is not, but is closely related to acceptable extended C, though
// not to acceptable C99.

class A { A (int); };

A::A (int i)
{
  int ar[1][i];    // { dg-error "7:ISO C\\+\\+ forbids variable length array .ar" }

  ar[0][0] = 0;
}

class B { B (int); };

B::B (int i)
{
  struct S {
    int ar[1][i];  // { dg-error "15:size of array .ar. is not an integral" }
  } s;

  s.ar[0][0] = 0;  // { dg-prune-output "no member" }
}

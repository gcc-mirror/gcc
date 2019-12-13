// PR c++/91363 - P0960R3: Parenthesized initialization of aggregates.
// { dg-do run { target c++2a } }

union U {
  int a;
  float b;
};

// u1 has no active member
U u1;
// u2 zero-initializes the first member, so u2.a is the active member and
// its value is 0.
U u2 = U();
// u3 uses non-list aggregate initialization, so u3.a is the active member
// and its value is 1.
U u3 = U(1);

int
main ()
{
  if (u2.a != 0)
    __builtin_abort ();
  if (u3.a != 1)
    __builtin_abort ();
}

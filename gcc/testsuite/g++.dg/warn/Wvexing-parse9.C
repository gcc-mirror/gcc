// PR c++/97881
// { dg-do compile }

void
cb ()
{
  volatile _Atomic (int) a1; // { dg-error "" }
}

// PR c++/92831
// { dg-do run { target c++11 } }
// { dg-options "" }

template<typename T> using id = T;
struct S { S () : s (false) { ++c; } S (bool x) : s (x) { ++c; } ~S () { --c; }; bool s; static int c; };
int S::c = 0;

void
foo (int i)
{
  const bool&& a
    = id<S[3]>{false, true, false}[i].s
      ? id<S[2]>{true, false}[i].s : id<S[4]>{true, false, true, false}[i].s;
  if (S::c != (i ? 2 : 4))
    __builtin_abort ();
}

void
baz (int i)
{
  const bool&& a = id<S[3]>{false, true, false}[i].s
		   ? : id<S[4]>{true, false, true, false}[i].s;
  if (S::c != (i ? 0 : 4))
    __builtin_abort ();
}

int
main ()
{
  foo (0);
  if (S::c != 0)
    __builtin_abort ();
  foo (1);
  if (S::c != 0)
    __builtin_abort ();
  baz (0);
  if (S::c != 0)
    __builtin_abort ();
  baz (1);
  if (S::c != 0)
    __builtin_abort ();
}

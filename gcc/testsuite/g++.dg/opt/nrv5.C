// PR c++/7279
// Test for the named return value optimization with inlining.
// Contributed by Jakub Jelinek <jakub@redhat.com>.
// { dg-do run }
// { dg-options -O2 }

enum E { E0, E1, E2, E3 };

struct S
{
  E s0 : 2;
  bool s1 : 1, s2 : 1, s3 : 1, s4 : 1, s5 : 1, s6 : 1;
  S ();
  void foo (E x);
};

S::S() : s1 (true), s2 (false), s0 (E1), s3 (true), s4 (false),
	 s5 (true), s6 (false) {}
void S::foo (E x) { this->s0 = x; }

inline S foo ()
{
  S s;
  s.foo (E0);
  return s;
}

inline S bar ()
{
  S s;
  s.foo (E2);
  return s;
}

void check (S &s, bool isfoo);

void test (bool isfoo)
{
  S a = isfoo ? foo () : bar ();
  check (a, isfoo);
}

extern "C" void abort ();

void check (S &s, bool isfoo)
{
  if (! s.s1 || s.s2 || ! s.s3 || s.s4 || ! s.s5 || s.s6)
    abort ();
  if (s.s0 != (isfoo ? E0 : E2))
    abort ();
}

int main ()
{
  test (true);
  test (false);
}

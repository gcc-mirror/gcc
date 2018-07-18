// { dg-do run } 
// { dg-options "-fpic" { target fpic } }

struct T
{
  bool a;
  T () : a (false) {}
  ~T () { if (!a) __builtin_abort (); }
};

__attribute__((noinline))
void
test (T &x)
{
  x.a = true;
}

int
main ()
{
  T T;
  test (T);
}


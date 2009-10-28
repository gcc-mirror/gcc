// PR debug/41801
// { dg-do compile }
// { dg-options "-O2 -g" }

struct T
{
  void
  foo () volatile
  {
    __sync_lock_release (&t);
    __sync_synchronize ();
  }
  bool t;
};

int
main ()
{
  T t = { false };
  t.foo ();
}

// { dg-do run }
// { dg-options "-fipa-pta -fnon-call-exceptions" }

struct Mutex
{
  bool locked;
  ~Mutex ()
#if __cplusplus <= 201402L
  throw(int)			// { dg-warning "deprecated" "" { target { c++11 && { ! c++17 } } } }
#else
  noexcept(false)
#endif
  {
    if (locked)
      throw 0;
  }
  void lock ()
  {
    locked = true;
  }
  void unlock ()
  {
    if (!locked)
      throw 0;
    locked = false;
  }
};

struct lock_guard
{
  Mutex *m;
  lock_guard (Mutex *m) : m(m)
  {
  }
  ~lock_guard ()
  {
    m->unlock ();
  }
};

int
main ()
{
  Mutex m;
  m.lock ();
  try
  {
    lock_guard l (&m);
  }
  catch ( ...)
  {
    __builtin_abort ();
  }
  return 0;
}

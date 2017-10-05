// { dg-additional-options -Wparentheses }

// Most Vexing Parse warnings
// in C++ anythig that syntactically looks like a decl IS a decl, this
// can lead to confused users, but worse silent unexpectedly unsafe
// code generation.

int (a); // { dg-warning "" }
int (*b);  // { dg-warning "" }
extern int (&c);  // { dg-warning "" }

int h1 = 0, h2 = 0;
struct H { H(...);};

namespace fns 
{
  int (*a) ();
  int (b) ();
  int (*c ()) ();
  int (d1 ()); // { dg-warning "" }
  int (d2 // { dg-warning "" }
       ());
  int (e) (int);
  int g (int (a)); // No warning because ...
  H h (int (h1), int (h2), 3); // ... not a function decl.
}

namespace arys
{
  int (*a)[1];
  int (b)[1];
  int (*c[1])[1];
  int (d1[1]); // { dg-warning "" }
  int (d2
       [1]);
  int (e[1])[1];
}

namespace complex
{
  int (*a())[1];
  int (*b[1])();
  int ((*c1())[1]); // { dg-warning "" }
  int ((*c2())
       [1]);
  int ((*d1[1])()); // { dg-warning "" }
  int ((*d2[1])	 // { dg-warning "" }
       ());
}

namespace motivation
{
  typedef int shared_mutex; // for exposition
  struct locker
  {
    locker ();
    locker (int &r);
    ~locker ();
  };
  class protected_state 
  {
    shared_mutex mutex; // not a real mutex type
    int state;

  public:
    void not_thread_safe ()
    {
      locker (mutex); // { dg-warning "" }
      state++; // oops
    }
    
    void thread_safe ()
    {
      locker lock (mutex);
      state++; // ok;
    }
  };
}

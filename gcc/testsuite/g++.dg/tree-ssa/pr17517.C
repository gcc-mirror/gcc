// Test PR 17517.  Test case provided by Serge Belyshev.
 
 /* { dg-do compile } */
 /* { dg-options "-O2" } */


extern void foo ();

struct Ptr {
  int * ptr;
  Ptr () { ptr = 0; }
  ~Ptr() { delete ptr; }
  Ptr &operator= (int * p) { ptr = p; return *this; }
};

int *new_checker () { foo (); return 0; }

void pipe (int c)
{
  Ptr checker;
  
  foo ();
  for (;;)
    {
    switch (c)
      {
    case '-':
      checker = new_checker ();
      break;
      }
    }
}

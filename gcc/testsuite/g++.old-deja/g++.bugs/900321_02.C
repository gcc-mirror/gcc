// { dg-do run  }
// g++ 1.37.1 bug 900321_02

// The following program exits with a nonzero status because the constructor
// is not called 3 times as it should be.  This program exits with a zero
// status when compiled with cfront 2.0.

// Cfront 2.0 passes this test.

// keywords: arrays, initialization, default constructor, operator new

int call_count = 0;

struct struct0 {
  struct0 ();
};

struct0::struct0 () { call_count++; }

typedef struct0 array[3];	// known dimension

int test ()
{
  new array;
  return (call_count != 3);
}

int main () { return test (); }

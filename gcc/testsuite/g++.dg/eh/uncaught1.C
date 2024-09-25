// PR libstdc++/10606
// { dg-do run }
// { dg-options "-Wno-deprecated" }
// { dg-options "-fuse-cxa-get-exception-ptr -Wno-deprecated" { target powerpc*-*-darwin* } }
// { dg-skip-if "requires hosted libstdc++ for cstdlib abort" { ! hostedlib } }

#include <exception>
#include <cstdlib>


struct Check {
  int obj1, obj2;
  bool state;
};

static Check const data[] = {
  { 0, 0, false },	// construct [0]
  { 1, 0, false  },	// [1] = [0]
  { 0, 0, true  },	// destruct [0]
  { 2, 1, true  },	// [2] = [1]
  { 2, 2, true  },      // destruct [2]
  { 3, 1, true  },	// [3] = [1]
  { 3, 3, false },	// destruct [3]
  { 1, 1, false },	// destruct [1]
  { 9, 9, false }	// end-of-data
};

static int pos = 0;

static void test(int obj1, int obj2, bool state)
{
  if (obj1 != data[pos].obj1) abort ();
  if (obj2 != data[pos].obj2) abort ();
  if (state != data[pos].state) abort ();
  pos++;
}


struct S {
  int id;
  S ();
  S (const S &);
  ~S ();
};

static int next_id = 0;

S::S()
  : id (next_id++)
{
  test (id, id, std::uncaught_exception ());
}

S::S(const S &x)
  : id (next_id++)
{
  test (id, x.id, std::uncaught_exception ());
}

S::~S()
{
  test (id, id, std::uncaught_exception ());
}

extern void foo (S *);

int main()
{
  try
    {
      try
	{
	  S s0;
	  throw s0;	// s1 is the exception object
	}
      catch (S s2)
	{
	  throw;
	}
    }
  catch (S s3)
    {
    }
 
  return 0;
}

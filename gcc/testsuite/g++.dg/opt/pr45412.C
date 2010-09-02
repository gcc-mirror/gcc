// { dg-do compile }
// { dg-options "-O2 -fipa-cp-clone -ftracer" }

int foo (int *);
void bar ();

struct S
{
  virtual int vm ();
  ~S ();
};

int
S::vm ()
{
  int state;
  switch (foo (&state))
    {
      case 0:
	  bar ();
      case 1:
	  delete this;
    }
  return state;
}


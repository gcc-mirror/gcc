// { dg-do compile }
/* { dg-options "-O1" }  */

extern "C" class A
{
};

template <typename T> class B:A
{
public:
    B (int *, T);
    ~B ()
    {
    }
};

bool a;

inline void
fn1 ()
{
  switch (0)
  case 0:
  {
    B <int*> b (0, 0);
    if (a)
      break;
  }
}

void
fn2 ()
{
  fn1 ();
}

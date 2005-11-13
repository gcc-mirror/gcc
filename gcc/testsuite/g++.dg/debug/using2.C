// PR c++/22489

namespace N { }

struct T
{
  T () { }
};

void
bar ()
{
  struct U : public T
  {
    void baz ()
    {
      using namespace N;
    }
  } u;
  u.baz();
}

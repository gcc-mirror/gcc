// PR c++/58701
// { dg-do run { target c++11 } }

static union
{
  union
  {
    int i = 7;
  };
};

extern "C" void abort(void);
int main()
{
  if (i != 7) abort();
  return 0;
}

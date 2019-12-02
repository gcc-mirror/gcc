template <class T>
struct foobar
{
  int xxx;
  T pes;
};

struct foo
{
  foobar<float> a;
};

foo myfoo;

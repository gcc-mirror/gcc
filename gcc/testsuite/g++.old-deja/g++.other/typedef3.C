//Build don't link:
template<class T>
struct X{
  T v;
  X(){}
  virtual ~X(){}
  virtual inline T f(T x){return x;}
};

void f()
{
  typedef int H;
  X<H> y;
}

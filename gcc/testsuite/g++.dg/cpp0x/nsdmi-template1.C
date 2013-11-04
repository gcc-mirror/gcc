// { dg-do run }
// { dg-options -std=c++11 }

struct base
{
  int calc_avg() { return 42; }
};

template <class T> struct nsdmi : T
{
  nsdmi() {}
  int avg() { return avg_; }
  int avg_ = this->calc_avg();
};

int main()
{
  nsdmi<base> x;
  if (x.avg() != 42)
    __builtin_abort();
}

// { dg-do compile }
// { dg-options "-fgnu-tm -O -fno-implicit-constexpr" }

class shared_count
{
public:
    volatile int j;
  shared_count() : j(0) { }
};

shared_count * c;
int main()
{
  __transaction_atomic {
    shared_count sc;
  }
  return 0;
}

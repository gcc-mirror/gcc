// PR c++/38958
// { dg-options "-Wunused" }

volatile int g;

struct Lock
{
  ~Lock() { g = 0; }
};

Lock AcquireLock() { return Lock(); }

int main()
{
  const Lock& lock = AcquireLock();
  g = 1;
  g = 2;
  g = 3;
}

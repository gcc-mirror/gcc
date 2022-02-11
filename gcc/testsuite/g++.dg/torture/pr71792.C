// { dg-do run { target c++11 } }
// PR C++/71792

class some_class
{
public:
  unsigned int np              : 4;
  unsigned int nc              : 8;
  unsigned int nc0             : 1;
};

template<bool what>
static void test_bug (const some_class &mp) {
  if (what) {
    int t = 0;
    for (auto i = mp.nc0; i < mp.nc; i++) {
      if (t != i) __builtin_abort ();
      t++;
    }
  }
}

static void test_ok (const some_class &mp) {
  int t = 0;
  for (auto i = mp.nc0; i < mp.nc; i++) {
    if (t != i) __builtin_abort ();
    t++;
  }
}

int main ()
{
  some_class mp;
  mp.nc0 = 0;
  mp.nc = 9;
  mp.np = 3;

  test_bug<true> (mp);
  test_ok (mp);

  return 0;
}

// { dg-options "" }

int n;

struct Y
{
  void f () {
    typedef int X[n];
    struct Z {
      X x; // { dg-error "variably modified" }
    };
  }
};

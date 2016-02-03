// { dg-do compile { target c++11 } }
// PR c++/69056

template <typename T, typename... Args>
void resolver(int (*) (T, Args...));

int funcA(int, float) { return 0; }
int funcA(double) { return 0; }

int funcB(int, float, char) { return 0; }
int funcB(int, bool) { return 0; }
int funcB(double) { return 0; }

int funcC(int) { return 0; }
int funcC(double) { return 0; }

void
foo (void)
{
  resolver (&funcA); // { dg-error "no match" }
  resolver<int> (&funcA);
  resolver<double> (&funcA);

  resolver<int> (&funcB); // { dg-error "no match" }
  resolver<int, char> (&funcB); // { dg-error "no match" }
  resolver<int, float> (&funcB);

  resolver<int> (&funcC);
  resolver<int, float> (&funcC); // { dg-error "no match" }
}

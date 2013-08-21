// PR c++/56130

int g_nn;
int& g_n __attribute__((deprecated)) = g_nn;

void f()
{
  int f_nn;
  int& f_n __attribute__((deprecated)) = f_nn;
  f_n = 1;    // { dg-warning "'f_n' is deprecated" }
}

int main()
{
  g_n = 1;    // { dg-warning "'g_n' is deprecated" }
  f();
}

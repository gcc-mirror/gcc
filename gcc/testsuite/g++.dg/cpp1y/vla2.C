// N3639 allows initialization and capture of VLAs
// { dg-do run { target c++11 } }
// { dg-options "-Wno-vla" }
// { dg-require-effective-target alloca }

void f(int n)
{
  int ar[n] = { 42 };
  auto l = [&] { return ar[0]; };
  if (l() != 42) __builtin_abort ();
}

int main()
{
  f(1);
}

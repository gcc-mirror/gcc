// PR c++/54583
// { dg-options "-Wunused-value" }

void fred()
{
  int n=10;
  double (*x)[n];
}

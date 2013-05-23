// PR c++/56930
// { dg-require-effective-target c++11 }
// { dg-options -Wconversion }

int main()
{
  int x = sizeof(int);
  int y { sizeof(int) };
}


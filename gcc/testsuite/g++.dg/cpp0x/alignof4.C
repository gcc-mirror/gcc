// PR c++/51316
// { dg-options "-std=c++11" }

int main()
{
  alignof(int []);
}

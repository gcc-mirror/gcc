// PR c++/51316
// { dg-options "-std=c++0x" }

int main()
{
  alignof(int []);
}

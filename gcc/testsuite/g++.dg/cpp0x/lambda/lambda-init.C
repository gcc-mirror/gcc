// Test for the explicit initializer extension
// { dg-options "-std=c++0x" }

int main()
{
  int j = [i = 2]{sizeof(i); return i;}();
  return (j != 2);
}

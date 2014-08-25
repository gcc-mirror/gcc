// Test for the explicit initializer extension of C++14
// { dg-do compile { target c++14 } }

int main()
{
  int j = [i = 2]{sizeof(i); return i;}();
  return (j != 2);
}

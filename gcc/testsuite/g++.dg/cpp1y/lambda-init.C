// Test for the explicit initializer extension of C++1y
// { dg-do compile { target c++1y } }

int main()
{
  int j = [i = 2]{sizeof(i); return i;}();
  return (j != 2);
}

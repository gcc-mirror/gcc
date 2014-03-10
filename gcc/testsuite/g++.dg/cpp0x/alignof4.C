// PR c++/51316
// { dg-do compile { target c++11 } }

int main()
{
  alignof(int []);
}

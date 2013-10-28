// PR c++/40689
// { dg-options "-std=c++11" }

class X
{
 public:
  X(): data {1,2,3,4,5} {}
 private:
  const short data[5];
};

int main()
{
  const float * pData = new const float[4] { 1.5, 2.5, 3.5, 4.5 };

  return 0;
}

// Core issue 901
// { dg-options "-std=c++11" }

struct A
{
  A(); ~A();
  void operator delete (void *) = delete;
  void operator delete[] (void *) = delete;
};

int main()
{
  A* ap = new A;
  ap = new A[2];
}

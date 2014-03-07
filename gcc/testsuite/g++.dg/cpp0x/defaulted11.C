// Core issue 901
// { dg-do compile { target c++11 } }

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

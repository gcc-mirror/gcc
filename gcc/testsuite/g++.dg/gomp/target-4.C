// { dg-do compile { target c++11 } }
// PR c++/120413

struct S
{
  S() {}
  ~S() {}
};

struct array
{
  S _arr[1];
};

int main()
{
#pragma omp target
  {
    array arr{};
  }
  return 0;
}

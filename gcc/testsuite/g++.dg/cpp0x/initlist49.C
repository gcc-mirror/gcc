// Test for non-trivial list-initialization with array new.
// { dg-do run { target c++11 } }

struct A
{
  enum E { c_string, number } e;
  A(const char *): e(c_string) {}
  A(int): e(number) {}
};

int main()
{
  A* ap = new A[2]{1, ""};
  if (ap[0].e != A::number || ap[1].e != A::c_string)
    return 1;
  delete[] ap;
}

// PR c++/50742

typedef int A;

void f(int i)
{
  switch (i)
    {
    case 0:
      using ::A;
    default:;
    }
}

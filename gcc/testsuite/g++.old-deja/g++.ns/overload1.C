// { dg-do run  }
// Unqualified lookup should find all functions.
// Duplicates are ignored as long as they lose during overload resolution.
namespace A{
  int f(){
    return 1;
  }
  int f(double);
}
namespace B{
  int f(int){
    return 2;
  }
  int f(double);
}

int f(int,int)
{
  return 3;
}

using namespace A;
using namespace B;

int main()
{
  if(f() != 1)
    return 1;
  if(f(1) != 2)
    return 1;
  if(f(0,0) != 3)
    return 1;
  return 0;
}

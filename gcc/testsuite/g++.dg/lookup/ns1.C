// PR c++/12862

typedef int Thingo;
  
namespace A
{
    void
    Thingo();
}
  
void
A::Thingo()
{
  ;
}
  
int
main()
{
  A::Thingo();
  return 0;
}

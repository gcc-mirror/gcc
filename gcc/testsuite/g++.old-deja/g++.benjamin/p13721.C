// Build don't link:
// prms-id: 13721

class A
{
  public :
  int a;
};
class B : public A
{
  public :
  void cmp(int a, int b) {}
  B(int a = 0)
    {
      cmp(A::a, a); //should not give warning
    }
};
int main(void)
{
  return(1);
}

// { dg-options "--std=c++0x" }
struct S
{
  S();
private:
  S(S const &&);
  S & operator=(S const &&);
};

void f()
{
  S a;
  S b(a);
  a = b;
}

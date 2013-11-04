// { dg-options "--std=c++11" }
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
  S b(a);			// { dg-error "deleted" }
  a = b;			// { dg-error "deleted" }
}

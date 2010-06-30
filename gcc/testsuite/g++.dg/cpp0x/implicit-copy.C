// { dg-options "--std=c++0x" }
struct S
{
  S();
private:
  S(S const &&);		// { dg-error "" }
  S & operator=(S const &&);	// { dg-error "" }
};

void f()
{
  S a;
  S b(a);			// { dg-error "" }
  a = b;			// { dg-error "" }
}

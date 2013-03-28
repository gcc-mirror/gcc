// { dg-do assemble  }
// prms-id: 807

extern "C" int printf(const char*, ...);

class B;

class AX
{
 protected:
   int x;

 public:
   operator B();
};


class B
{
 private:
   int x;
 public:
   B(const AX&);
};


int foo(B& b);			// { dg-message "" } referenced below


int main()
{
   AX a;
   foo(a);  // { dg-error "" } Ambiguous B(a) or a.operator B()  //  See ARM 12.3.2

}

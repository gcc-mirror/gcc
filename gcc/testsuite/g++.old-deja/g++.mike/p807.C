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


int foo(B& b);			// ERROR - referenced below


int main()
{
   AX a;
   foo(a);  // ERROR - Ambiguous B(a) or a.operator B()  //  See ARM 12.3.2

}

// Build don't link:
// prms-id: 438

class D;

class C
{
   public:
   void test() const;
};

class D
{
   public:
   void a(C& b); // ERROR - referenced below
};

void C::test() const
{
   D d;

   d.a(*this);	// ERROR - *this is const, so should get error
}

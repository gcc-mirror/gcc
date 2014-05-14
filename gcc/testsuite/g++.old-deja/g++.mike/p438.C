// { dg-do assemble  }
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
   void a(C& b); // { dg-message "D::a|no known conversion" }
};

void C::test() const
{
   D d;

   d.a(*this);	// { dg-error "const" } *this is const, so should get error
}

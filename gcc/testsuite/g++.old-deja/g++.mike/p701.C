// Build don't link:
// prms-id: 701

extern "C" 
{
  int printf(const char *, ...);
};


void Munge(int& x) 
{				// ERROR - referenced below
   x = 2;
}


class A 
{
 public:
   int i;
   A(int x) : i(x) {}
   void Safe() const;
};

void
A::Safe() const 
{
   Munge(i);	// ERROR - should not be able to modify a const object
}

int main()
{
   const A a(1);
   a.Safe();
}

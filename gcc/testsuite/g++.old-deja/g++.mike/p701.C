// { dg-do assemble  }
// prms-id: 701

extern "C" 
{
  int printf(const char *, ...);
}


void Munge(int& x) 	// { dg-error "passing argument 1" }
{
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
   Munge(i);	        // { dg-error "invalid initialization" }
}

int main()
{
   const A a(1);
   a.Safe();
}

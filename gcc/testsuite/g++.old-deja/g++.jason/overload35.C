// { dg-do assemble  }
// PRMS Id: 9647

class castBug
{
public:
   operator int *();
   operator const int *() const;
};

class castBug2
{
public:
   operator const int *() const;
};

void voidfn(void *);

void test()
{
   castBug b;
   castBug2 b2;
   voidfn(b);			// { dg-bogus "" } 
   voidfn(b2);			// { dg-error "" } discarding const
}

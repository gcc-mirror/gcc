// PRMS Id: 9647
// Build don't link:

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
   voidfn(b);			// gets bogus error
   voidfn(b2);			// ERROR - discarding const
}

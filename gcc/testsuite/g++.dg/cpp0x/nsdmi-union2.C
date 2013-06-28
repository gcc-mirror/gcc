// PR c++/52377
// { dg-require-effective-target c++11 }

union A				// { dg-error "multiple" }
{
  int i = 4;
  int j = 2;
};

A a;

union B
{
  int i,j;
  B(): i(1), j(2) {}		// { dg-error "multiple" }
};

B b;

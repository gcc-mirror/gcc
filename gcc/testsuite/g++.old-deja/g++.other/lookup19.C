// { dg-do assemble  }
// Test for proper handling of type lookup for conversion operator names.

// Test 1: Only at file scope
typedef int B;
struct A
{
  int B;
  operator B *(); // { dg-error "" } 
};

A::operator B * () // { dg-error "" } 
{
  return 0;
}

// Test 2: Only at class scope
struct C
{
  typedef int D;
  operator D *();
};

int D;
C::operator D * ()
{
  return 0;
}

// Test 3: Matching
struct E
{
  typedef int F;
  operator F *();
};

typedef int F;
E::operator F * ()
{
  return 0;
}

// Test 4: Conflicting
struct G
{
  typedef int H;
  operator H *();
};

typedef double H;
G::operator H * ()
{
  return 0;
}

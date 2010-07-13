// Circular implicit declarations were causing errors
// { dg-options -std=c++0x }

struct Ray;

struct Vector
{
  virtual void f();		// make non-trivially-copyable
  Vector();
  Vector(const Ray &) ;
};

struct array
{
  Vector v;
};

struct Ray
{
  array a;
  operator Vector();
};

extern Ray r1;
Ray r2=r1;

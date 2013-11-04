// Circular implicit declarations were causing errors
// { dg-options -std=c++11 }

struct Ray;

struct Vector
{
  virtual void f();		// make non-trivially-copyable
  Vector(const Ray &) ;
};

struct array
{
  Vector v;
};

struct Ray
{
  array a;
};

extern Ray r1;
Ray r2=r1;

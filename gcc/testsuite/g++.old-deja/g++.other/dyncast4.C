// Author: Alfred Miniarik <a8601248@unet.univie.ac.at>

// Even in a derived class, a private base cannot be dynamically downcasted
// from.

extern "C" void abort();

struct A {
  virtual ~A () {}
};

struct B : private A {
  B* a2b (A* objp)
  {
    return dynamic_cast<B*> (objp);
  }
};

int
main ()
{
  B b;
  A* aptr = (A*) &b;
  if (dynamic_cast <B*> (aptr)) abort ();
  if (b.a2b (aptr)) abort();
  return 0;
}

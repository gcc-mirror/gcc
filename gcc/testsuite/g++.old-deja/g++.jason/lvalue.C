// Bug: Scoped method calls don't propagate the constness of `this'.
// PRMS Id: 4181 (second testcase)
// Build don't link:

class D;

class Bptr
{
public:
  Bptr& operator=(D*);                          
  const Bptr& operator=(const D*) const;        
};

class Dptr : public Bptr
{
public:
  const Dptr& operator=(const D* rep) const 
  {
    Bptr::operator=(rep);
    return *this;
  }
};

// { dg-do run  }
extern "C" void abort();

int ic;

struct X 
{
  X() { ++ic; }
  X( const X & ) { ++ic; }
 ~X() { if (--ic < 0) abort(); }
};

struct V 
{
  virtual ~V() {}
};

struct A : public virtual V 
{
}; 

struct B : public virtual V 
{
  virtual void foo( X ) = 0;
}; 

struct D : public A, public virtual B 
{
  virtual void foo( X ) {}
}; 

int main()
{
  B *b = new D;
  b->foo( X() );
}

// Origin: Mark Mitchell <mark@codesourcery.com>

extern "C" void abort ();

struct B;

struct S 
{
  S (B*);
  ~S ();

  B* b_;
};

struct B 
{
  B () : s (this) { }
      
  virtual void f () { }
  
  S s;
};

S::S (B* b) : b_ (b) { }

S::~S () { b_->f (); }
  
struct D : public B
{
  virtual void f () { abort (); }
};

int main ()
{
  D d;
}

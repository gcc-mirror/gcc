// { dg-do run  }
// Origin: Mark Mitchell <mark@codesourcery.com>

struct B 
{
  B ();
  virtual void f () = 0;
};

B::B () 
{
}

extern B* bp;

template <class T>
struct C : public B
{
  virtual void f () 
    {
    }
};

template <class T>
struct D : public B
{
  virtual void f () 
    {
      bp = new C<T*>;
    }
};

B* bp = new D<int>;

int main ()
{
  bp->f ();
  bp->f ();
}



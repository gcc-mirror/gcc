// Test PR 17400.  Test case provided by Serge Belyshev.  

/* { dg-do compile } */
/* { dg-options "-O1" } */


void inc (int &);
bool dec_test (int &);

struct A
{
  int c;
  
  friend void AddRef (A * p)
  {
    inc (p->c);
  }
  
  friend void Release (A * p)
  {
    if(dec_test (p->c))
      delete p;
  }
};

struct B
{
  B (A *p) : obj(p)
  {
    AddRef (obj);
  }
  
  ~B()
  {
    Release (obj);
  }
  
  void swap (B &rhs)
  {
    A * tmp = obj;
    obj = rhs.obj;
    rhs.obj = tmp;
  }
  
  A *obj;
};

void bar (A *p1, A* p2)
{
    B px (p1);
    B px2 (p2);
    px.swap (px2);
}

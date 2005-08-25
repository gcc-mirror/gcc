// { dg-do assemble  }
// 981203 bkoz
// g++/13908

class chile
{
public:
protected:
private:
};

typedef void (chile::* pmf) ();

void* foo;

void bar (chile* pobj, pmf pmethod)
{
  //-edg: expected member name
  //-g++: taking address of bound pointer-to-member expression
  foo = (void*) &(pobj->*pmethod);  // { dg-error "invalid use" } 
}

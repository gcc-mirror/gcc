// { dg-do assemble  }
// PRMS Id: 4357
// Bug: g++ forgets to clear out push/popclass cache stuff when instantiating
// templates.

template <class T> class ccHandle { };

class cc_GStack
{
  static cc_GStack* freeList;
};

// OK if ccGStack is not derived from ccHandle<something>
class ccGStack : public ccHandle<int> { };

struct S { };

S* freeList;	  

class X
{
public:
    void foo();
};

void X::foo()
{
  S m;
  freeList = &m;
}
